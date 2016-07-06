#include <cstdio>
#include <cassert>
#include <cstdlib>
#include "legion.h"
#include "realm.h"
#include "id.h"

#include "test_mapper.h"
#include "default_mapper.h"

using namespace Legion;
using namespace Legion::Mapping;
using namespace LegionRuntime::Accessor;
using namespace LegionRuntime::Arrays;

struct ReportMapper : public DefaultMapper {

  ReportMapper(Machine machine,
      Runtime *rt, Processor local) :
        DefaultMapper(rt->get_mapper_runtime(), machine, local)
      , local_mapped(false)
      , mapper_proc(Processor::NO_PROC) {}

  // We assert both tasks are mapped on the same proc
  bool local_mapped;
  Processor mapper_proc;
  std::vector<Processor> permutation;

  virtual void slice_task(const MapperContext ctx,
                          const Task& task,
                          const SliceTaskInput& input,
                                SliceTaskOutput& output)
  {
    assert(task.is_index_space);
    size_t middle;
    Machine::ProcessorQuery cpus(machine);
#define tname_is(tn) (strcmp(task.get_task_name(),tn)==0)
    if (!tname_is("write") && !tname_is("read")) goto def;
    cpus.only_kind(Processor::LOC_PROC);
    if(task.current_proc != cpus.first() || local_mapped){
        output.slices.push_back(TaskSlice(input.domain, task.current_proc, false, false));
        return;
      }
    middle = cpus.count()/2;
    local_mapped = true;

    if(tname_is("write")) {
        assert(mapper_proc == Processor::NO_PROC);
        mapper_proc = task.current_proc;
        printf("From Mapper! We have the following CPUs:\n");
        for (auto p : cpus) printf("\t%llx\n", p.id);
        permutation.resize(cpus.count());
        std::copy(cpus.begin(), cpus.end(), permutation.begin());
        std::random_device rd;
        std::shuffle(permutation.begin(), permutation.end(), std::mt19937(rd()));
        printf("Permuted to:\n");
        for (auto p : permutation) printf("\t%llx\n", p.id);

#define __MK_SLICES(__plus_off)                 \
        assert(input.domain.get_dim() == 1);    \
        auto rect = input.domain.get_rect<1>(); \
        assert(rect.dim_size(0) == middle);     \
        output.slices.resize(middle);           \
        size_t idx = 0;                         \
        for (LegionRuntime::Arrays::GenericPointInRectIterator<1> pir(rect); \
              pir; pir++, idx++)                \
        {                                       \
          Rect<1> slice(pir.p, pir.p);          \
          output.slices[idx] = TaskSlice(       \
                Domain::from_rect<1>(slice)     \
              , permutation[idx __plus_off]     \
              , false                           \
              , false);                         \
        }
        __MK_SLICES()
      }
    else if(tname_is("read")) {
        assert(mapper_proc == task.current_proc);
        __MK_SLICES(+middle)
#undef __MK_SLICES
        // Back to nothing
        mapper_proc = Processor::NO_PROC;
      }
    return;

    def:
    DefaultMapper::slice_task(ctx, task, input, output);
  } // slice_task

};

void mapper_registration(Machine machine, Runtime *rt,
                          const std::set<Processor> &local_procs)
{
  for (const Processor & p : local_procs)
    rt->replace_default_mapper(
          new ReportMapper(machine, rt, p), p
        );
}

extern "C" void reg_mappers(){
  Runtime::set_registration_callback(mapper_registration);
}
