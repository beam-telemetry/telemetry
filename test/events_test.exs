defmodule EventsTest do
  use ExUnit.Case

  defmodule TestHandler do
    def echo_event(event, value, metadata, %{send_to: pid} = config) do
      send(pid, {:event, event, value, metadata, config})
    end

    def raise_on_event(_event, _value, _metadata, _config) do
      raise "Got an event"
    end
  end

  setup do
    handler_id = :crypto.strong_rand_bytes(16) |> Base.encode16()

    on_exit fn ->
      Events.detach(handler_id)
    end

    {:ok, handler_id: handler_id}
  end

  test "attaching returns error if handler with the same ID already exist", %{
    handler_id: handler_id
  } do
    :ok = Events.attach(handler_id, [:some, :event], TestHandler, :echo_event)

    assert {:error, :already_exists} =
             Events.attach(handler_id, [:some, :event], TestHandler, :echo_event)
  end

  test "attached mf is called when event exactly matches the prefix", %{
    handler_id: handler_id
  } do
    event = [:a, :test, :event]
    config = %{send_to: self()}
    value = 1
    metadata = %{some: :metadata}
    Events.attach(handler_id, event, TestHandler, :echo_event, config)

    Events.execute(event, value, metadata)

    assert_receive {:event, ^event, ^value, ^metadata, ^config}
  end

  test "attached mf is not called when event is a prefix of the prefix handler is attached to", %{
    handler_id: handler_id
  } do
    event = [:a, :test]
    prefix = [:a, :test, :prefix]
    config = %{send_to: self()}
    value = 1
    metadata = %{some: :metadata}
    Events.attach(handler_id, prefix, TestHandler, :echo_event, config)

    Events.execute(event, value, metadata)

    refute_receive {:event, ^event, ^value, ^metadata, ^config}
  end

  test "event metadata is an empty map by default", %{handler_id: handler_id} do
    event = [:a, :test, :event]
    config = %{send_to: self()}
    value = 1
    Events.attach(handler_id, event, TestHandler, :echo_event, config)

    Events.execute(event, value)

    assert_receive {:event, ^event, ^value, metadata, ^config}
    assert %{} == metadata
  end

  test "handlers attached to event can be listed", %{handler_id: handler_id} do
    event = [:a, :test, :event]
    config = %{send_to: self()}
    Events.attach(handler_id, event, TestHandler, :echo_event, config)

    assert [{handler_id, event, TestHandler, :echo_event, config}] == Events.list_handlers(event)
  end

  test "handlers attached to event prefix can be listed", %{handler_id: handler_id} do
    prefix1 = []
    prefix2 = [:a]
    prefix3 = [:a, :test]
    event = [:a, :test, :event]
    config = %{send_to: self()}
    Events.attach(handler_id, event, TestHandler, :echo_event, config)

    for prefix <- [prefix1, prefix2, prefix3] do
      assert [{handler_id, event, TestHandler, :echo_event, config}] ==
               Events.list_handlers(prefix)
    end

    assert [] == Events.list_handlers(event ++ [:something])
  end

  @tag :capture_log
  test "mf is detached when it fails", %{handler_id: handler_id} do
    event = [:a, :test, :event]
    Events.attach(handler_id, event, TestHandler, :raise_on_event)

    Events.execute(event, 1)

    assert [] == Events.list_handlers(event)
  end

  test "detached mf is not called when handlers are executed", %{handler_id: handler_id} do
    event = [:a, :test, :event]
    config = %{send_to: self()}
    value = 1
    metadata = %{some: :metadata}
    Events.attach(handler_id, event, TestHandler, :echo_event, config)

    Events.detach(handler_id)
    Events.execute(event, value, metadata)

    refute_receive {:event, ^event, ^value, ^metadata, ^config}
  end

  test "detaching returns error if handler with given ID doesn't exist", %{
    handler_id: handler_id
  } do
    assert {:error, :not_found} = Events.detach(handler_id)
  end

  test "mf attached to event prefix is called when handlers are executed", %{
    handler_id: handler_id
  } do
    prefix = [:a, :test]
    event = [:a, :test, :event]
    config = %{send_to: self()}
    value = 1
    metadata = %{some: :metadata}
    Events.attach(handler_id, prefix, TestHandler, :echo_event, config)

    Events.execute(event, value, metadata)

    assert_receive {:event, ^event, ^value, ^metadata, ^config}
  end
end
