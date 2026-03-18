/*
 * coreutils-main.c — Entry point for the jerboa-coreutils multi-call binary.
 *
 * All 108 coreutils are embedded in a single self-contained ELF binary.
 * Dispatch is done by the Scheme dispatch.ss program based on argv[0] basename.
 *
 * Chez boot files (petite.boot, scheme.boot, coreutils.boot) and the compiled
 * dispatch program are embedded as C byte arrays — no external files needed.
 *
 * The C functions from libcoreutils.c are statically linked here; the Scheme
 * modules call (load-shared-object #f) to resolve them from this binary.
 * -rdynamic ensures these symbols are exported for dlsym/RTLD_DEFAULT lookups.
 *
 * Threading note: We use Sscheme_script (not Sscheme_start) so that the
 * program runs outside the boot file and can create threads if needed.
 */

#define _GNU_SOURCE
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/mman.h>
#include "scheme.h"
#include "cu_program.h"     /* coreutils dispatch .so */
#include "cu_petite.h"      /* petite.boot */
#include "cu_scheme.h"      /* scheme.boot */
#include "cu_boot.h"        /* coreutils.boot (all libs) */

int main(int argc, char *argv[]) {
    /* Write the compiled dispatch program to a memfd */
    int fd = memfd_create("jerboa-coreutils", MFD_CLOEXEC);
    if (fd < 0) { perror("memfd_create"); return 1; }
    if (write(fd, cu_program_data, cu_program_size) != (ssize_t)cu_program_size) {
        perror("write memfd"); close(fd); return 1;
    }
    char prog_path[64];
    snprintf(prog_path, sizeof(prog_path), "/proc/self/fd/%d", fd);

    /* Initialize Chez Scheme runtime */
    Sscheme_init(NULL);

    /* Register embedded boot files */
    Sregister_boot_file_bytes("petite",    (void*)cu_petite_data,  cu_petite_size);
    Sregister_boot_file_bytes("scheme",    (void*)cu_scheme_data,  cu_scheme_size);
    Sregister_boot_file_bytes("coreutils", (void*)cu_boot_data,    cu_boot_size);

    /* Build heap from registered boot files (libraries only) */
    Sbuild_heap(NULL, NULL);

    /* Pass argv[0] (command name) via env; Sscheme_script drops it from command-line */
    setenv("_CU_CMD", argv[0], 1);

    /* Run dispatch program */
    int status = Sscheme_script(prog_path, argc, (const char**)argv);

    close(fd);
    Sscheme_deinit();
    return status;
}
