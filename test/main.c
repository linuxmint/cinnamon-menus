#include <time.h>
#include <stdio.h>

#include "cmenu-tree.h"

int
main ()
{
    CMenuTree *tree;
    GError *error = NULL;
    clock_t start = clock ();

    tree = cmenu_tree_new ("cinnamon-applications.menu", CMENU_TREE_FLAGS_INCLUDE_NODISPLAY);
    cmenu_tree_load_sync (tree, &error);

    printf("Loaded in %f ms\n", ((double) (clock () - start)) * 1000 / CLOCKS_PER_SEC);
}
