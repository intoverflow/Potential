
#include <stdio.h>

struct IntDesc_t {
  unsigned long Offset0_15 : 16;
  unsigned long SegmentSel : 16;

  unsigned long IST     : 3;   // Set to 0
  unsigned long Unused0 : 5;   // Set to 0
  unsigned long type    : 4;   // GateType_t, use 1110b for interrupt gate
  unsigned long Unused1 : 1;   // Set to 0
  unsigned long dpl     : 2;   // dpl
  unsigned long p       : 1;   // present

  unsigned short        Offset16_31 : 16;

  unsigned long         Offset32_64 : 32;
  unsigned long         Reserved : 32;
} __attribute__((packed));

void changeDpl1(int dpl, struct IntDesc_t *pIntDesc) {
  pIntDesc->dpl = dpl;
}

void changeDpl2(int dpl, struct IntDesc_t *pIntDesc);

void changeDoubleFaultOffsetHi1(unsigned long offsethi, struct IntDesc_t *pIntDescs) {
  struct IntDesc_t *pDoubleFault = &pIntDescs[8];
  pDoubleFault->Offset32_64 = offsethi;
}

void changeDoubleFaultOffsetHi2(unsigned long offsethi, struct IntDesc_t *pIntDescs);

int main() {
  struct IntDesc_t intDesc;
  struct IntDesc_t intDescs[32];
  unsigned short dpl;

  printf("an intgate has size %ld bytes, so that entry 8 has offset 0x%lx\n",
		sizeof(intDesc), (unsigned long)&intDescs[8] - (unsigned long)intDescs);

  changeDpl1(0, &intDesc);
  dpl = getDpl2(&intDesc);
  printf("dpl: %d (%d)\n", intDesc.dpl, dpl);

  changeDpl1(1, &intDesc);
  dpl = getDpl2(&intDesc);
  printf("dpl: %d (%d)\n", intDesc.dpl, dpl);

  changeDpl2(3, &intDesc);
  dpl = getDpl2(&intDesc);
  printf("dpl: %d (%d)\n", intDesc.dpl, dpl);

  changeDpl2(2, &intDesc);
  dpl = getDpl2(&intDesc);
  printf("dpl: %d (%d)\n", intDesc.dpl, dpl);

  intDescs[8].Offset32_64 = 0;
  printf("offset_hi of doubleFault: 0x%.8x\n", intDescs[8].Offset32_64);
  changeDoubleFaultOffsetHi2(0xdeadbeef, intDescs);
  printf("offset_hi of doubleFault: 0x%.8x\n", intDescs[8].Offset32_64);

  return 0;
}
