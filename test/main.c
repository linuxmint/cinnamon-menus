#include <time.h>
#include <stdio.h>
#include <unistd.h>

#include "cmenu-tree.h"



int
main (int argc, char *argv[])
{
    int args;
    int print_tree = FALSE;
    CMenuTree *tree;
    GError *error = NULL;
    CMenuTreeDirectory *root;
    CMenuTreeIter *iter;
    CMenuTreeItemType cat_next_type;
    clock_t start;

    while ((args = getopt(argc, argv, "p")) != -1) {
        switch (args)
        {
            case 'p':
                print_tree = TRUE;
                break;
            default:
                printf ("Usage: %s [-p]\n", argv[0]);
                return 1;
        }
    }

    start = clock ();

    tree = cmenu_tree_new ("cinnamon-applications.menu", CMENU_TREE_FLAGS_INCLUDE_NODISPLAY);
    cmenu_tree_load_sync (tree, &error);

    printf ("Loaded in %f ms\n", ((double) (clock () - start)) * 1000 / CLOCKS_PER_SEC);

    if (print_tree) {
        printf("\nMenu tree:\n");
    }

    root = cmenu_tree_get_root_directory (tree);
    iter = cmenu_tree_directory_iter (root);

    while ((cat_next_type = cmenu_tree_iter_next (iter)) != CMENU_TREE_ITEM_INVALID)
    {
        gpointer item = NULL;

        switch (cat_next_type)
        {
            case CMENU_TREE_ITEM_ENTRY:
                {
                    const char *id;

                    item = cmenu_tree_iter_get_entry (iter);
                    id = cmenu_tree_entry_get_name (item);

                    printf ("error: category %s is actually an entry not a directory!\n", id);

                    return 2;
                }
            case CMENU_TREE_ITEM_DIRECTORY:
                {
                    CMenuTreeDirectory *category;
                    CMenuTreeIter *cat_iter;
                    CMenuTreeItemType app_next_type;

                    category = cmenu_tree_iter_get_directory (iter);

                    if (print_tree)
                    {
                        printf ("  %s (category)\n", cmenu_tree_directory_get_name (category));
                    }

                    cat_iter = cmenu_tree_directory_iter (category);
                    while ((app_next_type = cmenu_tree_iter_next (cat_iter)) != CMENU_TREE_ITEM_INVALID)
                    {
                        switch (app_next_type)
                        {
                            case CMENU_TREE_ITEM_ENTRY:
                                {
                                    CMenuTreeEntry *entry = cmenu_tree_iter_get_entry (cat_iter);

                                    if (print_tree)
                                    {
                                        printf ("  |---%s (app)\n", cmenu_tree_entry_get_name (entry));
                                    }
                                }
                                break;
                            case CMENU_TREE_ITEM_DIRECTORY:
                                {
                                    CMenuTreeDirectory *directory = cmenu_tree_iter_get_directory (cat_iter);

                                    printf ("error: entry %s is actually a directory not an entry!\n", cmenu_tree_directory_get_name (directory));

                                    return 2;
                                }
                            default:
                                break;
                        }
                    }

                    break;
                }
            default:
                break;
        }
        if (item != NULL)
            cmenu_tree_item_unref (item);
    }
}
