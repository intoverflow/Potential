
#include <stdio.h>

struct IntDesc_t {
  unsigned short Offset0_15;
  unsigned short SegmentSel;

  unsigned short IST     : 3;   // Set to 0
  unsigned short Unused0 : 5;   // Set to 0
  unsigned short type    : 4;   // GateType_t, use 1110b for interrupt gate
  unsigned short Unused1 : 1;   // Set to 0
  unsigned short dpl     : 2;   // dpl
  unsigned short p       : 1;   // present

  unsigned short        Offset16_31;

  unsigned long         Offset32_64;
  unsigned long         Reserved;
} __attribute__((packed));

void changeDpl1(int dpl, struct IntDesc_t *pIntDesc) {
  pIntDesc->dpl = dpl;
}

void changeDpl2(int dpl, struct IntDesc_t *pIntDesc);

int main() {
  int dpl1 = 0;
  int dpl2 = 1;
  int dpl3 = 3;
  struct IntDesc_t intDesc;

  changeDpl1(dpl1, &intDesc);

  // printf("dpl: %d\n", intDesc.dpl);

  changeDpl1(dpl2, &intDesc);

  // printf("dpl: %d\n", intDesc.dpl);

  // changeDpl2(dpl3, &intDesc);

  // printf("dpl: %d\n", intDesc.dpl);

  return 0;
}
