#   infra/libf2wrap/CMakeLists.txt
cmake_minimum_required(VERSION 3.12)
project(libf2wrap C)
find_package(PkgConfig REQUIRED)
find_library(FPGA_PCI NAMES fpga_pci REQUIRED)
add_library(f2wrap SHARED f2wrap.c)
target_link_libraries(f2wrap PRIVATE ${FPGA_PCI})
set_target_properties(f2wrap PROPERTIES OUTPUT_NAME f2wrap)
install(TARGETS f2wrap LIBRARY DESTINATION lib)

#   infra/libf2wrap/f2wrap.c
#include <fpga_pci.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
static int bar_handle = -1;
int f2_open(int slot) {
  return fpga_pci_attach(slot, FPGA_APP_PF, APP_PF_BAR0, 0, &bar_handle);
}
int f2_poke(uint32_t off, uint32_t val) {
  return fpga_pci_poke(bar_handle, off, val);
}
int f2_peek(uint32_t off, uint32_t *val) {
  return fpga_pci_peek(bar_handle, off, val);
}