#include "cmenu-tree.h"

int
main ()
{
    CMenuTree *tree;
    GError *error = NULL;

    tree = cmenu_tree_new ("cinnamon-applications.menu", CMENU_TREE_FLAGS_INCLUDE_NODISPLAY);
    cmenu_tree_load_sync (tree, &error);
}
