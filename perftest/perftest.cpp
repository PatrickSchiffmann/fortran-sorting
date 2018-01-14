#include <random>
#include <functional>
#include <benchmark/benchmark.h>

const int range_min = 1;
const int range_max = 65536;
const int range_multiplier = 4;

const int rand_seed = 42;
const int rand_min = -1e6;
const int rand_max = 1e6;


extern "C" {
    void insertionsort(int N, int* values);
    void insertionsort_kv(int N, int* values, int* keys);

    void mergesort(int N, int* values);
    void mergesort_kv(int N, int* values, int* keys);

    void quicksort(int N, int* values);
    void quicksort_kv(int N, int* values, int* keys);
}

inline std::function<int(void)> setup_rand() {
    std::default_random_engine generator;
    std::uniform_int_distribution<int> distribution(rand_min, rand_max);
    return std::bind(distribution, generator);
}

inline void fill(int N, int* values, std::function<int(void)> rand) {
    for(int i = 0; i < N; ++i)
        values[i] = rand();
}

static void data_creation(benchmark::State& state) {
    const int N = state.range(0);
    int* values = (int*) malloc(sizeof(int) * N);
    auto rand = setup_rand();
    for (auto _ : state) {
        fill(N, values, rand);
    }
}
BENCHMARK(data_creation)->RangeMultiplier(range_multiplier)->Range(range_min, range_max);

static void bench_insertionsort(benchmark::State& state) {
    const int N = state.range(0);
    int* values = (int*) malloc(sizeof(int) * N);
    auto rand = setup_rand();
    for (auto _ : state) {
        fill(N, values, rand);
        insertionsort(N, values);
    }
}
BENCHMARK(bench_insertionsort)->RangeMultiplier(range_multiplier)->Range(range_min, range_max);

static void bench_mergesort(benchmark::State& state) {
    const int N = state.range(0);
    int* values = (int*) malloc(sizeof(int) * N);
    auto rand = setup_rand();
    for (auto _ : state) {
        fill(N, values, rand);
        mergesort(N, values);
    }
}
BENCHMARK(bench_mergesort)->RangeMultiplier(range_multiplier)->Range(range_min, range_max);

static void bench_quicksort(benchmark::State& state) {
    const int N = state.range(0);
    int* values = (int*) malloc(sizeof(int) * N);
    auto rand = setup_rand();
    for (auto _ : state) {
        fill(N, values, rand);
        quicksort(N, values);
    }
}
BENCHMARK(bench_quicksort)->RangeMultiplier(range_multiplier)->Range(range_min, range_max);

BENCHMARK_MAIN();

