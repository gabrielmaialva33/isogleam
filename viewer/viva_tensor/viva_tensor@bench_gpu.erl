-module(viva_tensor@bench_gpu).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/viva_tensor/bench_gpu.gleam").
-export([main/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(
    " Benchmark Comparativo: viva_tensor (Pure Gleam) vs viva_glands (Rust/Candle GPU)\n"
    "\n"
    " Run: gleam run -m viva_tensor/bench_gpu\n"
).

-file("src/viva_tensor/bench_gpu.gleam", 48).
-spec safe_glands_check() -> {ok, binary()} | {error, nil}.
safe_glands_check() ->
    {error, nil}.

-file("src/viva_tensor/bench_gpu.gleam", 40).
-spec check_glands() -> boolean().
check_glands() ->
    case safe_glands_check() of
        {ok, _} ->
            true;

        {error, _} ->
            false
    end.

-file("src/viva_tensor/bench_gpu.gleam", 213).
-spec bench_similarity_comparative() -> nil.
bench_similarity_comparative() ->
    gleam_stdlib:println(<<"(Comparativo requer viva_glands carregado)"/utf8>>).

-file("src/viva_tensor/bench_gpu.gleam", 218).
-spec bench_dot_comparative() -> nil.
bench_dot_comparative() ->
    gleam_stdlib:println(<<"(Comparativo requer viva_glands carregado)"/utf8>>).

-file("src/viva_tensor/bench_gpu.gleam", 222).
-spec bench_matmul_comparative() -> nil.
bench_matmul_comparative() ->
    gleam_stdlib:println(<<"(Comparativo requer viva_glands carregado)"/utf8>>).

-file("src/viva_tensor/bench_gpu.gleam", 54).
-spec run_comparative_benchmarks() -> nil.
run_comparative_benchmarks() ->
    gleam_stdlib:println(<<"━━━ SIMILARITY BENCHMARK ━━━"/utf8>>),
    bench_similarity_comparative(),
    gleam_stdlib:println(<<"\n━━━ DOT PRODUCT BENCHMARK ━━━"/utf8>>),
    bench_dot_comparative(),
    gleam_stdlib:println(<<"\n━━━ MATMUL BENCHMARK ━━━"/utf8>>),
    bench_matmul_comparative().

-file("src/viva_tensor/bench_gpu.gleam", 226).
-spec float_to_string(float()) -> binary().
float_to_string(F) ->
    Rounded = erlang:float(erlang:round(F * 100.0)) / 100.0,
    gleam_stdlib:float_to_string(Rounded).

-file("src/viva_tensor/bench_gpu.gleam", 231).
-spec format_number(integer()) -> binary().
format_number(N) ->
    case N >= 1000000 of
        true ->
            <<(erlang:integer_to_binary(N div 1000000))/binary, "M"/utf8>>;

        false ->
            case N >= 1000 of
                true ->
                    <<(erlang:integer_to_binary(N div 1000))/binary, "K"/utf8>>;

                false ->
                    erlang:integer_to_binary(N)
            end
    end.

-file("src/viva_tensor/bench_gpu.gleam", 65).
-spec run_gleam_only_benchmarks() -> nil.
run_gleam_only_benchmarks() ->
    Sizes = [100, 500, 1000, 2000, 4096, 8192],
    gleam_stdlib:println(<<"━━━ PURE GLEAM PERFORMANCE ━━━\n"/utf8>>),
    gleam_stdlib:println(<<"DOT PRODUCT (cosine similarity base):"/utf8>>),
    gleam@list:each(
        Sizes,
        fun(N) ->
            A = viva_tensor@tensor:random_uniform([N]),
            B = viva_tensor@tensor:random_uniform([N]),
            {Time_us, _} = timer:tc(
                fun() ->
                    _ = viva_tensor@tensor:dot(A, B),
                    nil
                end
            ),
            Ops_per_sec = case Time_us > 0 of
                true ->
                    case erlang:float(Time_us) of
                        +0.0 -> +0.0;
                        -0.0 -> -0.0;
                        Gleam@denominator -> 1000000.0 / Gleam@denominator
                    end;

                false ->
                    999999.0
            end,
            gleam_stdlib:println(
                <<<<<<<<<<<<"  "/utf8, (erlang:integer_to_binary(N))/binary>>/binary,
                                    "d: "/utf8>>/binary,
                                (erlang:integer_to_binary(Time_us))/binary>>/binary,
                            "μs ("/utf8>>/binary,
                        (float_to_string(Ops_per_sec))/binary>>/binary,
                    " ops/s)"/utf8>>
            )
        end
    ),
    gleam_stdlib:println(<<"\nMATMUL (matrix sizes):"/utf8>>),
    Mat_sizes = [32, 64, 128, 256],
    gleam@list:each(
        Mat_sizes,
        fun(N@1) ->
            A@1 = viva_tensor@tensor:ones([N@1, N@1]),
            B@1 = viva_tensor@tensor:ones([N@1, N@1]),
            _ = viva_tensor@tensor:matmul(A@1, B@1),
            Times = begin
                _pipe = gleam@list:range(1, 5),
                gleam@list:map(
                    _pipe,
                    fun(_) ->
                        {T, _} = timer:tc(
                            fun() ->
                                _ = viva_tensor@tensor:matmul(A@1, B@1),
                                nil
                            end
                        ),
                        T
                    end
                )
            end,
            Avg_time = gleam@list:fold(Times, 0, fun(Acc, T@1) -> Acc + T@1 end)
            div 5,
            Flops = ((2 * N@1) * N@1) * N@1,
            Gflops = case Avg_time > 0 of
                true ->
                    (case erlang:float(Avg_time) of
                        +0.0 -> +0.0;
                        -0.0 -> -0.0;
                        Gleam@denominator@1 -> erlang:float(Flops) / Gleam@denominator@1
                    end) / 1000.0;

                false ->
                    +0.0
            end,
            gleam_stdlib:println(
                <<<<<<<<<<<<<<<<"  "/utf8,
                                                (erlang:integer_to_binary(N@1))/binary>>/binary,
                                            "x"/utf8>>/binary,
                                        (erlang:integer_to_binary(N@1))/binary>>/binary,
                                    ": "/utf8>>/binary,
                                (erlang:integer_to_binary(Avg_time))/binary>>/binary,
                            "μs ("/utf8>>/binary,
                        (float_to_string(Gflops))/binary>>/binary,
                    " GFLOPS)"/utf8>>
            )
        end
    ),
    gleam_stdlib:println(<<"\nREDUCTIONS (sum, mean, max):"/utf8>>),
    Large_sizes = [1000, 10000, 100000, 1000000],
    gleam@list:each(
        Large_sizes,
        fun(N@2) ->
            T@2 = viva_tensor@tensor:random_uniform([N@2]),
            {Sum_time, _} = timer:tc(
                fun() ->
                    _ = viva_tensor@tensor:sum(T@2),
                    nil
                end
            ),
            {Mean_time, _} = timer:tc(
                fun() ->
                    _ = viva_tensor@tensor:mean(T@2),
                    nil
                end
            ),
            {Max_time, _} = timer:tc(
                fun() ->
                    _ = viva_tensor@tensor:max(T@2),
                    nil
                end
            ),
            Throughput = case Sum_time > 0 of
                true ->
                    case erlang:float(Sum_time) of
                        +0.0 -> +0.0;
                        -0.0 -> -0.0;
                        Gleam@denominator@2 -> erlang:float(N@2) / Gleam@denominator@2
                    end;

                false ->
                    +0.0
            end,
            gleam_stdlib:println(
                <<<<<<<<<<<<<<<<<<<<"  "/utf8, (format_number(N@2))/binary>>/binary,
                                                    ": sum="/utf8>>/binary,
                                                (erlang:integer_to_binary(
                                                    Sum_time
                                                ))/binary>>/binary,
                                            "μs, mean="/utf8>>/binary,
                                        (erlang:integer_to_binary(Mean_time))/binary>>/binary,
                                    "μs, max="/utf8>>/binary,
                                (erlang:integer_to_binary(Max_time))/binary>>/binary,
                            "μs ("/utf8>>/binary,
                        (float_to_string(Throughput))/binary>>/binary,
                    "M elem/s)"/utf8>>
            )
        end
    ),
    gleam_stdlib:println(
        <<"\n╔══════════════════════════════════════════════════════════════════╗"/utf8>>
    ),
    gleam_stdlib:println(
        <<"║                    PURE GLEAM SUMMARY                            ║"/utf8>>
    ),
    gleam_stdlib:println(
        <<"╠══════════════════════════════════════════════════════════════════╣"/utf8>>
    ),
    gleam_stdlib:println(
        <<"║  ✅ Zero dependencies - runs anywhere BEAM runs                  ║"/utf8>>
    ),
    gleam_stdlib:println(
        <<"║  ✅ Type-safe with Result types                                  ║"/utf8>>
    ),
    gleam_stdlib:println(
        <<"║  ✅ Named tensors for semantic clarity                           ║"/utf8>>
    ),
    gleam_stdlib:println(
        <<"║  ⚡ For GPU acceleration: use viva_glands backend                ║"/utf8>>
    ),
    gleam_stdlib:println(
        <<"╚══════════════════════════════════════════════════════════════════╝"/utf8>>
    ).

-file("src/viva_tensor/bench_gpu.gleam", 14).
-spec main() -> nil.
main() ->
    gleam_stdlib:println(
        <<"╔══════════════════════════════════════════════════════════════════╗"/utf8>>
    ),
    gleam_stdlib:println(
        <<"║  BENCHMARK: viva_tensor (Gleam) vs viva_glands (Rust/Candle)    ║"/utf8>>
    ),
    gleam_stdlib:println(
        <<"╚══════════════════════════════════════════════════════════════════╝\n"/utf8>>
    ),
    Glands_available = check_glands(),
    case Glands_available of
        true ->
            gleam_stdlib:println(
                <<"✅ viva_glands (Rust/Candle) disponível!\n"/utf8>>
            ),
            run_comparative_benchmarks();

        false ->
            gleam_stdlib:println(
                <<"⚠️  viva_glands não disponível - rodando só Pure Gleam\n"/utf8>>
            ),
            run_gleam_only_benchmarks()
    end.
