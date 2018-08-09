defmodule TelemetryTest do
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
      Telemetry.detach(handler_id)
    end

    {:ok, handler_id: handler_id}
  end

  test "attaching returns error if handler with the same ID already exist", %{
    handler_id: handler_id
  } do
    :ok = Telemetry.attach(handler_id, [:some, :event], TestHandler, :echo_event)

    assert {:error, :already_exists} =
             Telemetry.attach(handler_id, [:some, :event], TestHandler, :echo_event)
  end

  test "handler is invoked when event it's attached to is emitted", %{
    handler_id: handler_id
  } do
    event = [:a, :test, :event]
    config = %{send_to: self()}
    value = 1
    metadata = %{some: :metadata}
    Telemetry.attach(handler_id, event, TestHandler, :echo_event, config)

    Telemetry.execute(event, value, metadata)

    assert_receive {:event, ^event, ^value, ^metadata, ^config}
  end

  test "event metadata is an empty map by default", %{handler_id: handler_id} do
    event = [:a, :test, :event]
    config = %{send_to: self()}
    value = 1
    Telemetry.attach(handler_id, event, TestHandler, :echo_event, config)

    Telemetry.execute(event, value)

    assert_receive {:event, ^event, ^value, metadata, ^config}
    assert %{} == metadata
  end

  test "handlers attached to event can be listed", %{handler_id: handler_id} do
    event = [:a, :test, :event]
    config = %{send_to: self()}
    Telemetry.attach(handler_id, event, TestHandler, :echo_event, config)

    assert [{handler_id, event, TestHandler, :echo_event, config}] ==
             Telemetry.list_handlers(event)
  end

  test "handlers attached to event prefix can be listed", %{handler_id: handler_id} do
    prefix1 = []
    prefix2 = [:a]
    prefix3 = [:a, :test]
    event = [:a, :test, :event]
    config = %{send_to: self()}
    Telemetry.attach(handler_id, event, TestHandler, :echo_event, config)

    for prefix <- [prefix1, prefix2, prefix3] do
      assert [{handler_id, event, TestHandler, :echo_event, config}] ==
               Telemetry.list_handlers(prefix)
    end

    assert [] == Telemetry.list_handlers(event ++ [:something])
  end

  @tag :capture_log
  test "mf is detached when it fails", %{handler_id: handler_id} do
    event = [:a, :test, :event]
    Telemetry.attach(handler_id, event, TestHandler, :raise_on_event)

    Telemetry.execute(event, 1)

    assert [] == Telemetry.list_handlers(event)
  end

  test "detached mf is not called when handlers are executed", %{handler_id: handler_id} do
    event = [:a, :test, :event]
    config = %{send_to: self()}
    value = 1
    metadata = %{some: :metadata}
    Telemetry.attach(handler_id, event, TestHandler, :echo_event, config)

    Telemetry.detach(handler_id)
    Telemetry.execute(event, value, metadata)

    refute_receive {:event, ^event, ^value, ^metadata, ^config}
  end

  test "detaching returns error if handler with given ID doesn't exist", %{
    handler_id: handler_id
  } do
    assert {:error, :not_found} = Telemetry.detach(handler_id)
  end

  test "handler is not invoked when prefix of the event it's attached to is emitted", %{
    handler_id: handler_id
  } do
    prefix = [:a, :test]
    event = [:a, :test, :event]
    config = %{send_to: self()}
    value = 1
    metadata = %{some: :metadata}
    Telemetry.attach(handler_id, event, TestHandler, :echo_event, config)

    Telemetry.execute(prefix, value, metadata)

    refute_receive {:event, ^event, ^value, ^metadata, ^config}
  end

  test "handler is not invoked when event more specific than the one it's attached to is emitted",
       %{
         handler_id: handler_id
       } do
    event = [:a, :test, :event]
    more_specific_event = [:a, :test, :event, :specific]
    config = %{send_to: self()}
    value = 1
    metadata = %{some: :metadata}
    Telemetry.attach(handler_id, event, TestHandler, :echo_event, config)

    Telemetry.execute(more_specific_event, value, metadata)

    refute_receive {:event, ^event, ^value, ^metadata, ^config}
  end

  test "handler can be attached to many events at once", %{handler_id: handler_id} do
    event1 = [:a, :first, :event]
    event2 = [:a, :second, :event]
    event3 = [:a, :third, :event]
    config = %{send_to: self()}
    value = 1
    metadata = %{some: :metadata}
    Telemetry.attach_many(handler_id, [event1, event2, event3], TestHandler, :echo_event, config)

    Telemetry.execute(event1, value, metadata)
    Telemetry.execute(event2, value, metadata)
    Telemetry.execute(event3, value, metadata)

    for event <- [event1, event2, event3] do
      assert_receive {:event, ^event, ^value, ^metadata, ^config}
    end
  end

  @tag :capture_log
  test "handler attached to many events at once is detached on failure of any invokation", %{
    handler_id: handler_id
  } do
    event1 = [:a, :first, :event]
    event2 = [:a, :second, :event]
    event3 = [:a, :third, :event]
    config = %{send_to: self()}
    value = 1
    metadata = %{some: :metadata}

    Telemetry.attach_many(
      handler_id,
      [event1, event2, event3],
      TestHandler,
      :raise_on_event,
      config
    )

    Telemetry.execute(event1, value, metadata)

    for event <- [event1, event2, event3] do
      assert [] == Telemetry.list_handlers(event)
    end
  end

  test "handler attached to many events at once can be listed", %{handler_id: handler_id} do
    event1 = [:a, :first, :event]
    event2 = [:a, :second, :event]
    event3 = [:a, :third, :event]
    config = %{send_to: self()}
    Telemetry.attach_many(handler_id, [event1, event2, event3], TestHandler, :echo_event, config)

    for event <- [event1, event2, event3] do
      assert [{handler_id, event, TestHandler, :echo_event, config}] ==
               Telemetry.list_handlers(event)
    end
  end

  test "handler attached to many events at once is detached from all of them", %{
    handler_id: handler_id
  } do
    event1 = [:a, :first, :event]
    event2 = [:a, :second, :event]
    event3 = [:a, :third, :event]
    config = %{send_to: self()}
    Telemetry.attach_many(handler_id, [event1, event2, event3], TestHandler, :echo_event, config)

    Telemetry.detach(handler_id)

    for event <- [event1, event2, event3] do
      assert [] == Telemetry.list_handlers(event)
    end
  end
end
