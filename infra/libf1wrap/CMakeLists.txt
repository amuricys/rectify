#   infra/libf1wrap/CMakeLists.txt
cmake_minimum_required(VERSION 3.12)
project(libf1wrap C)
find_package(PkgConfig REQUIRED)
find_library(FPGA_PCI NAMES fpga_pci REQUIRED)
add_library(f1wrap SHARED f1wrap.c)
target_link_libraries(f1wrap PRIVATE ${FPGA_PCI})
set_target_properties(f1wrap PROPERTIES OUTPUT_NAME f1wrap)
install(TARGETS f1wrap LIBRARY DESTINATION lib)

#   infra/libf1wrap/f1wrap.c
#include <fpga_pci.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
static int bar_handle = -1;
int f1_open(int slot) {
  return fpga_pci_attach(slot, FPGA_APP_PF, APP_PF_BAR0, 0, &bar_handle);
}
int f1_poke(uint32_t off, uint32_t val) {
  return fpga_pci_poke(bar_handle, off, val);
}
int f1_peek(uint32_t off, uint32_t *val) {
  return fpga_pci_peek(bar_handle, off, val);
}