SECTIONS
{
    /* generate symbols pointing to the start and end of the stackmap section */
    /* ELF specific */
    .llvm_stackmaps :
    {
        __stackmaps_start = .;
        KEEP(*(.llvm_stackmaps))
        __stackmaps_end = .;
    }
}
INSERT AFTER .text;
