defmodule EventsTest do
  use ExUnit.Case

  defmodule TestSubscriber do
    def echo_event(event, value, %{send_to: pid} = config) do
      send(pid, {:event, event, value, config})
    end

    def raise_on_event(_event, _value, _config) do
      raise "Got an event"
    end
  end

  setup do
    subscription_id = :crypto.strong_rand_bytes(16) |> Base.encode16()

    on_exit fn ->
      Events.unsubscribe(subscription_id)
    end

    {:ok, sub_id: subscription_id}
  end

  test "subscribing returns error if subscription with the same ID already exist", %{
    sub_id: sub_id
  } do
    :ok = Events.subscribe(sub_id, [:some, :event], TestSubscriber, :echo_event)

    assert {:error, :already_exists} =
             Events.subscribe(sub_id, [:some, :event], TestSubscriber, :echo_event)
  end

  test "subscribed mf is called when event exactly matches the subscription prefix", %{
    sub_id: sub_id
  } do
    event = [:a, :test, :event]
    config = %{send_to: self()}
    value = 1
    Events.subscribe(sub_id, event, TestSubscriber, :echo_event, config)

    Events.emit(event, value)

    assert_receive {:event, ^event, ^value, ^config}
  end

  test "subscribed mf is not called when event is a prefix of the subscription prefix", %{
    sub_id: sub_id
  } do
    event = [:a, :test]
    prefix = [:a, :test, :prefix]
    config = %{send_to: self()}
    value = 1
    Events.subscribe(sub_id, prefix, TestSubscriber, :echo_event, config)

    Events.emit(event, value)

    refute_receive {:event, ^event, ^value, ^config}
  end

  test "subscriptions to event can be listed", %{sub_id: sub_id} do
    event = [:a, :test, :event]
    config = %{send_to: self()}
    Events.subscribe(sub_id, event, TestSubscriber, :echo_event, config)

    assert [{sub_id, event, TestSubscriber, :echo_event, config}] ==
             Events.list_subscriptions(event)
  end

  test "subscriptions to event prefix can be listed", %{sub_id: sub_id} do
    prefix1 = []
    prefix2 = [:a]
    prefix3 = [:a, :test]
    event = [:a, :test, :event]
    config = %{send_to: self()}
    Events.subscribe(sub_id, event, TestSubscriber, :echo_event, config)

    for prefix <- [prefix1, prefix2, prefix3] do
      assert [{sub_id, event, TestSubscriber, :echo_event, config}] ==
               Events.list_subscriptions(prefix)
    end

    assert [] == Events.list_subscriptions(event ++ [:something])
  end

  test "mf is unsubsribed when it fails", %{sub_id: sub_id} do
    event = [:a, :test, :event]
    Events.subscribe(sub_id, event, TestSubscriber, :raise_on_event)

    Events.emit(event, 1)

    assert [] == Events.list_subscriptions(event)
  end

  test "unsubcribed mf is not called when event is emitted", %{sub_id: sub_id} do
    event = [:a, :test, :event]
    config = %{send_to: self()}
    value = 1
    Events.subscribe(sub_id, event, TestSubscriber, :echo_event, config)

    Events.unsubscribe(sub_id)
    Events.emit(event, value)

    refute_receive {:event, ^event, ^value, ^config}
  end

  test "unsubscribing returns error if subscription doesn't exist", %{
    sub_id: sub_id
  } do
    assert {:error, :not_found} = Events.unsubscribe(sub_id)
  end

  test "mf subscribed to event prefix is called when event is emitted", %{sub_id: sub_id} do
    prefix = [:a, :test]
    event = [:a, :test, :event]
    config = %{send_to: self()}
    value = 1
    Events.subscribe(sub_id, prefix, TestSubscriber, :echo_event, config)

    Events.emit(event, value)

    assert_receive {:event, ^event, ^value, ^config}
  end
end
