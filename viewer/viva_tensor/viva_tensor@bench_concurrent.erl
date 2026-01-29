-module(viva_tensor@bench_concurrent).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/viva_tensor/bench_concurrent.gleam").
-export([main/0]).
-export_type([pid_/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(
    " Benchmark de Concorrência - Onde Gleam BRILHA!\n"
    "\n"
    " O poder do BEAM: milhões de processos leves\n"
    " C/C++ libs são rápidas em single-thread, mas Gleam escala!\n"
    "\n"
    " Run: gleam run -m viva_tensor/bench_concurrent\n"
).

-type pid_() :: any().

-file("src/viva_tensor/bench_concurrent.gleam", 254).
-spec float_to_string(float()) -> binary().
float_to_string(F) ->
    Rounded = erlang:float(erlang:round(F * 100.0)) / 100.0,
    gleam_stdlib:float_to_string(Rounded).

-file("src/viva_tensor/bench_concurrent.gleam", 105).
-spec bench_parallel_reductions() -> nil.
bench_parallel_reductions() ->
    Tensors = begin
        _pipe = gleam@list:range(1, 1000),
        gleam@list:map(
            _pipe,
            fun(_) -> viva_tensor@tensor:random_uniform([1000]) end
        )
    end,
    {Seq_time, _} = timer:tc(
        fun() ->
            gleam@list:map(Tensors, fun(T) -> viva_tensor@tensor:sum(T) end)
        end
    ),
    {Par_time, _} = timer:tc(
        fun() ->
            Parent = erlang:self(),
            gleam@list:each(
                Tensors,
                fun(T@1) ->
                    erlang:spawn(
                        fun() ->
                            Result = viva_tensor@tensor:sum(T@1),
                            viva_tensor_ffi:send_msg(Parent, Result)
                        end
                    )
                end
            ),
            viva_tensor_ffi:collect_n(1000)
        end
    ),
    Speedup = case Par_time > 0 of
        true ->
            case erlang:float(Par_time) of
                +0.0 -> +0.0;
                -0.0 -> -0.0;
                Gleam@denominator -> erlang:float(Seq_time) / Gleam@denominator
            end;

        false ->
            1.0
    end,
    gleam_stdlib:println(
        <<<<<<<<<<<<"  1000 tensors x 1000 elementos: seq="/utf8,
                                (erlang:integer_to_binary(Seq_time div 1000))/binary>>/binary,
                            "ms, par="/utf8>>/binary,
                        (erlang:integer_to_binary(Par_time div 1000))/binary>>/binary,
                    "ms, speedup="/utf8>>/binary,
                (float_to_string(Speedup))/binary>>/binary,
            "x"/utf8>>
    ).

-file("src/viva_tensor/bench_concurrent.gleam", 144).
-spec bench_parallel_similarity() -> nil.
bench_parallel_similarity() ->
    Query = viva_tensor@tensor:random_uniform([512]),
    Documents = begin
        _pipe = gleam@list:range(1, 10000),
        gleam@list:map(
            _pipe,
            fun(_) -> viva_tensor@tensor:random_uniform([512]) end
        )
    end,
    gleam_stdlib:println(<<"  Query vs 10K documents (512d embeddings):"/utf8>>),
    {Seq_time, _} = timer:tc(
        fun() ->
            gleam@list:map(
                Documents,
                fun(Doc) -> viva_tensor@tensor:dot(Query, Doc) end
            )
        end
    ),
    {Par_time, _} = timer:tc(
        fun() ->
            Parent = erlang:self(),
            Chunks = gleam@list:sized_chunk(Documents, 100),
            Num_chunks = erlang:length(Chunks),
            gleam@list:each(
                Chunks,
                fun(Chunk) ->
                    erlang:spawn(
                        fun() ->
                            Results = gleam@list:map(
                                Chunk,
                                fun(Doc@1) ->
                                    viva_tensor@tensor:dot(Query, Doc@1)
                                end
                            ),
                            viva_tensor_ffi:send_msg(Parent, Results)
                        end
                    )
                end
            ),
            viva_tensor_ffi:collect_n(Num_chunks)
        end
    ),
    Speedup = case Par_time > 0 of
        true ->
            case erlang:float(Par_time) of
                +0.0 -> +0.0;
                -0.0 -> -0.0;
                Gleam@denominator -> erlang:float(Seq_time) / Gleam@denominator
            end;

        false ->
            1.0
    end,
    Throughput = case Par_time > 0 of
        true ->
            case (erlang:float(Par_time) / 1000000.0) of
                +0.0 -> +0.0;
                -0.0 -> -0.0;
                Gleam@denominator@1 -> 10000.0 / Gleam@denominator@1
            end;

        false ->
            +0.0
    end,
    gleam_stdlib:println(
        <<<<"    Sequential: "/utf8,
                (erlang:integer_to_binary(Seq_time div 1000))/binary>>/binary,
            "ms"/utf8>>
    ),
    gleam_stdlib:println(
        <<<<<<<<"    Parallel:   "/utf8,
                        (erlang:integer_to_binary(Par_time div 1000))/binary>>/binary,
                    "ms (speedup: "/utf8>>/binary,
                (float_to_string(Speedup))/binary>>/binary,
            "x)"/utf8>>
    ),
    gleam_stdlib:println(
        <<<<"    Throughput: "/utf8, (float_to_string(Throughput))/binary>>/binary,
            " queries/sec"/utf8>>
    ).

-file("src/viva_tensor/bench_concurrent.gleam", 259).
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

-file("src/viva_tensor/bench_concurrent.gleam", 60).
-spec bench_parallel_creation() -> nil.
bench_parallel_creation() ->
    Counts = [100, 1000, 10000],
    gleam@list:each(
        Counts,
        fun(N) ->
            {Seq_time, _} = timer:tc(fun() -> _pipe = gleam@list:range(1, N),
                    gleam@list:map(
                        _pipe,
                        fun(_) -> viva_tensor@tensor:random_uniform([100]) end
                    ) end),
            {Par_time, _} = timer:tc(
                fun() ->
                    Parent = erlang:self(),
                    _pipe@1 = gleam@list:range(1, N),
                    gleam@list:each(
                        _pipe@1,
                        fun(_) ->
                            erlang:spawn(
                                fun() ->
                                    Result = viva_tensor@tensor:random_uniform(
                                        [100]
                                    ),
                                    viva_tensor_ffi:send_msg(Parent, Result)
                                end
                            )
                        end
                    ),
                    viva_tensor_ffi:collect_n(N)
                end
            ),
            Speedup = case Par_time > 0 of
                true ->
                    case erlang:float(Par_time) of
                        +0.0 -> +0.0;
                        -0.0 -> -0.0;
                        Gleam@denominator -> erlang:float(Seq_time) / Gleam@denominator
                    end;

                false ->
                    1.0
            end,
            gleam_stdlib:println(
                <<<<<<<<<<<<<<<<"  "/utf8, (format_number(N))/binary>>/binary,
                                            " tensors: seq="/utf8>>/binary,
                                        (erlang:integer_to_binary(
                                            Seq_time div 1000
                                        ))/binary>>/binary,
                                    "ms, par="/utf8>>/binary,
                                (erlang:integer_to_binary(Par_time div 1000))/binary>>/binary,
                            "ms, speedup="/utf8>>/binary,
                        (float_to_string(Speedup))/binary>>/binary,
                    "x"/utf8>>
            )
        end
    ).

-file("src/viva_tensor/bench_concurrent.gleam", 196).
-spec bench_process_spawning() -> nil.
bench_process_spawning() ->
    gleam_stdlib:println(
        <<"  Quantos processos BEAM conseguimos spawnar?"/utf8>>
    ),
    Counts = [1000, 10000, 100000],
    gleam@list:each(
        Counts,
        fun(N) ->
            {Time, _} = timer:tc(
                fun() ->
                    Parent = erlang:self(),
                    _pipe = gleam@list:range(1, N),
                    gleam@list:each(
                        _pipe,
                        fun(_) ->
                            erlang:spawn(
                                fun() -> viva_tensor_ffi:send_msg(Parent, 1) end
                            )
                        end
                    ),
                    viva_tensor_ffi:collect_n(N)
                end
            ),
            Spawns_per_sec = case Time > 0 of
                true ->
                    case (erlang:float(Time) / 1000000.0) of
                        +0.0 -> +0.0;
                        -0.0 -> -0.0;
                        Gleam@denominator -> erlang:float(N) / Gleam@denominator
                    end;

                false ->
                    +0.0
            end,
            gleam_stdlib:println(
                <<<<<<<<<<<<"    "/utf8, (format_number(N))/binary>>/binary,
                                    " processos: "/utf8>>/binary,
                                (erlang:integer_to_binary(Time div 1000))/binary>>/binary,
                            "ms ("/utf8>>/binary,
                        (float_to_string(Spawns_per_sec))/binary>>/binary,
                    " spawns/sec)"/utf8>>
            )
        end
    ),
    gleam_stdlib:println(<<""/utf8>>),
    gleam_stdlib:println(
        <<"  💡 Em C/C++ você precisaria de pthreads, mutex, condition vars..."/utf8>>
    ),
    gleam_stdlib:println(
        <<"  💡 Em Gleam: erlang_spawn() e pronto! Zero data races garantido."/utf8>>
    ).

-file("src/viva_tensor/bench_concurrent.gleam", 14).
-spec main() -> nil.
main() ->
    gleam_stdlib:println(
        <<"╔══════════════════════════════════════════════════════════════════╗"/utf8>>
    ),
    gleam_stdlib:println(
        <<"║  CONCURRENCY BENCHMARK - Onde BEAM/Gleam BRILHA vs C/C++        ║"/utf8>>
    ),
    gleam_stdlib:println(
        <<"╚══════════════════════════════════════════════════════════════════╝\n"/utf8>>
    ),
    gleam_stdlib:println(
        <<"C/C++ libs (Eigen, OpenBLAS, MKL) são rápidas em single-thread..."/utf8>>
    ),
    gleam_stdlib:println(
        <<"Mas quantos tensors você processa em PARALELO? 🤔\n"/utf8>>
    ),
    gleam_stdlib:println(<<"━━━ TEST 1: Criação Paralela de Tensors ━━━"/utf8>>),
    bench_parallel_creation(),
    gleam_stdlib:println(<<"\n━━━ TEST 2: Reduções Paralelas ━━━"/utf8>>),
    bench_parallel_reductions(),
    gleam_stdlib:println(
        <<"\n━━━ TEST 3: Similaridade em Batch (Embedding Search) ━━━"/utf8>>
    ),
    bench_parallel_similarity(),
    gleam_stdlib:println(<<"\n━━━ TEST 4: BEAM Process Spawning ━━━"/utf8>>),
    bench_process_spawning(),
    gleam_stdlib:println(
        <<"\n╔══════════════════════════════════════════════════════════════════╗"/utf8>>
    ),
    gleam_stdlib:println(
        <<"║  CONCLUSÃO: BEAM escala horizontalmente, C/C++ escala vertical  ║"/utf8>>
    ),
    gleam_stdlib:println(
        <<"║  Para ML inference em produção: Gleam + Rust NIF = 🔥           ║"/utf8>>
    ),
    gleam_stdlib:println(
        <<"╚══════════════════════════════════════════════════════════════════╝"/utf8>>
    ).
