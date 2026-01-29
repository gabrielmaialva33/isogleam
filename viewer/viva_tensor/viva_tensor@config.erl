-module(viva_tensor@config).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/viva_tensor/config.gleam").
-export([default_config/0, performance_config/0, memory_config/0, gpu_config/0, should_use_strided/3, ensure_optimal/3, for_matmul/1, for_reduction/1, for_indexing/1]).
-export_type([operation_type/0, tensor_config/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(
    " Auto-Backend Selector - Smart tensor backend selection\n"
    "\n"
    " Automatically chooses optimal backend (list vs strided) based on:\n"
    " - Operation type (sequential, random access, matrix)\n"
    " - Tensor size\n"
    " - User configuration\n"
    "\n"
    " Benchmarked on RTX 4090 + BEAM VM\n"
).

-type operation_type() :: sequential | random_access | matrix_op.

-type tensor_config() :: {tensor_config,
        integer(),
        integer(),
        boolean(),
        boolean()}.

-file("src/viva_tensor/config.gleam", 45).
?DOC(" Default configuration based on benchmarks\n").
-spec default_config() -> tensor_config().
default_config() ->
    {tensor_config, 500, 64, false, false}.

-file("src/viva_tensor/config.gleam", 55).
?DOC(" High-performance config (prefer strided for large tensors)\n").
-spec performance_config() -> tensor_config().
performance_config() ->
    {tensor_config, 100, 32, false, false}.

-file("src/viva_tensor/config.gleam", 65).
?DOC(" Memory-efficient config (prefer lists)\n").
-spec memory_config() -> tensor_config().
memory_config() ->
    {tensor_config, 5000, 256, false, false}.

-file("src/viva_tensor/config.gleam", 75).
?DOC(" GPU-optimized config (always strided for batched ops)\n").
-spec gpu_config() -> tensor_config().
gpu_config() ->
    {tensor_config, 64, 16, false, false}.

-file("src/viva_tensor/config.gleam", 121).
?DOC(" Get shape from tensor (helper for pattern matching)\n").
-spec get_tensor_shape(viva_tensor@tensor:tensor()) -> list(integer()).
get_tensor_shape(T) ->
    case T of
        {tensor, _, Shape} ->
            Shape;

        {strided_tensor, _, Shape@1, _, _} ->
            Shape@1
    end.

-file("src/viva_tensor/config.gleam", 89).
?DOC(" Check if should use strided backend for given operation\n").
-spec should_use_strided(
    viva_tensor@tensor:tensor(),
    operation_type(),
    tensor_config()
) -> boolean().
should_use_strided(T, Op, Config) ->
    case {erlang:element(4, Config), erlang:element(5, Config)} of
        {true, _} ->
            true;

        {_, true} ->
            false;

        {false, false} ->
            Tensor_size = viva_tensor@tensor:size(T),
            case Op of
                sequential ->
                    false;

                random_access ->
                    Tensor_size >= erlang:element(2, Config);

                matrix_op ->
                    Shape = get_tensor_shape(T),
                    case Shape of
                        [Rows, Cols] ->
                            (Rows * Cols) >= erlang:element(3, Config);

                        _ ->
                            Tensor_size >= erlang:element(3, Config)
                    end
            end
    end.

-file("src/viva_tensor/config.gleam", 129).
?DOC(" Ensure tensor is in optimal format for operation\n").
-spec ensure_optimal(
    viva_tensor@tensor:tensor(),
    operation_type(),
    tensor_config()
) -> viva_tensor@tensor:tensor().
ensure_optimal(T, Op, Config) ->
    Use_strided = should_use_strided(T, Op, Config),
    case {T, Use_strided} of
        {{strided_tensor, _, _, _, _}, true} ->
            T;

        {{tensor, _, _}, false} ->
            T;

        {{tensor, _, _}, true} ->
            viva_tensor@tensor:to_strided(T);

        {{strided_tensor, _, _, _, _}, false} ->
            viva_tensor@tensor:to_contiguous(T)
    end.

-file("src/viva_tensor/config.gleam", 152).
?DOC(" Auto-optimize tensor for matmul\n").
-spec for_matmul(viva_tensor@tensor:tensor()) -> viva_tensor@tensor:tensor().
for_matmul(T) ->
    ensure_optimal(T, matrix_op, default_config()).

-file("src/viva_tensor/config.gleam", 157).
?DOC(" Auto-optimize tensor for reduction operations\n").
-spec for_reduction(viva_tensor@tensor:tensor()) -> viva_tensor@tensor:tensor().
for_reduction(T) ->
    ensure_optimal(T, sequential, default_config()).

-file("src/viva_tensor/config.gleam", 162).
?DOC(" Auto-optimize tensor for indexing\n").
-spec for_indexing(viva_tensor@tensor:tensor()) -> viva_tensor@tensor:tensor().
for_indexing(T) ->
    ensure_optimal(T, random_access, default_config()).
