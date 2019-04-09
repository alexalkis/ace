cmake_minimum_required(VERSION 3.13)
project(ace C)

set(CMAKE_C_STANDARD 11)

include_directories(src)

add_executable(ace
        src/acedef.h
        src/alloc.c
        src/assign.c
        src/basfun.c
        src/control.c
        src/declare.c
        src/event.c
        src/expr.c
        src/factor.c
        src/file.c
        src/gadget.c
        src/gfx.c
        src/iff.c
        src/lex.c
        src/lexvar.c
        src/lexvar.h
        src/libfunc.c
        src/memory.c
        src/menu.c
        src/message.c
        src/misc.c
        src/opt.c
        src/parse.c
        src/parsevar.c
        src/print.c
        src/screen.c
        src/serial.c
        src/statement.c
        src/structs.h
        src/sub.c
        src/sym.c
        src/symvar.c
        src/symvar.h
        src/ver.c
        src/warns.c
        src/window.c)

target_link_libraries(ace m)