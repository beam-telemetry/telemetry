defmodule Events.Impl.Agent do
  @moduledoc false
  # Dummy implementation, not really performant but good for designing the APIs.

  @behaviour Events.Impl

  @impl true
  def child_spec(_) do
    %{
      id: __MODULE__,
      start: {Agent, :start_link, [fn -> %{} end, [name: __MODULE__]]}
    }
  end

  @impl true
  def subscribe(sub_id, prefix, module, function, config) do
    Agent.get_and_update(__MODULE__, fn subs ->
      if Map.has_key?(subs, sub_id) do
        {:error, subs}
      else
        {:ok, Map.put(subs, sub_id, {sub_id, prefix, module, function, config})}
      end
    end)
  end

  @impl true
  def unsubscribe(sub_id) do
    Agent.get_and_update(__MODULE__, fn subs ->
      if Map.has_key?(subs, sub_id) do
        {:ok, Map.delete(subs, sub_id)}
      else
        {:error, subs}
      end
    end)
  end

  @impl true
  def list_subscribed_to(event) do
    subs = Agent.get(__MODULE__, fn subs -> Map.values(subs) end)

    Enum.filter(subs, fn {_, prefix, _, _, _} ->
      Enum.take(event, length(prefix)) == prefix
    end)
  end

  @impl true
  def list_subscriptions(prefix) do
    subs = Agent.get(__MODULE__, fn subs -> Map.values(subs) end)

    prefix_len = length(prefix)

    Enum.filter(subs, fn {_, sub_prefix, _, _, _} ->
      Enum.take(sub_prefix, prefix_len) == prefix
    end)
  end
end
