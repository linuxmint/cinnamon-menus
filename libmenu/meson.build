public_headers = [
	'gmenu-tree.h',
]

public_sources = [
    'gmenu-tree.c',
    public_headers,
]

libmenu_private_headers = [
    'desktop-entries.h',
    'entry-directories.h',
    'menu-layout.h',
    'menu-monitor.h',
    'menu-util.h',
]

libmenu_sources = [
    'desktop-entries.c',
    'entry-directories.c',
    'menu-layout.c',
    'menu-monitor.c',
    'menu-util.c',
    public_sources,
    libmenu_private_headers,
]

libmenu_deps = [
    gio,
    config_h,
]

libcinnamon_menus = library(
    'cinnamon-menu-3',
    libmenu_sources,
    soversion: binary_major_version,
    version: binary_version,
    include_directories: include_root,
    dependencies: libmenu_deps,
    install: true,
    build_by_default: false,
)

cmenu_dep = declare_dependency(
    include_directories: include_directories('.'),
    link_with: libcinnamon_menus,
    dependencies: libmenu_deps,
    link_args: ['-Wl,-Bsymbolic', '-Wl,-z,relro', '-Wl,-z,now'],
)

install_headers(
    public_headers,
    subdir: 'cinnamon-menus-3.0'
)

pkgconfig = import('pkgconfig')

# meson 0.46.0 can drop the version keyword and move libraries to a
# positional argument
pkgconfig.generate(
    name: 'libcinnamon-menu-3.0',
    description: 'Desktop Menu Specification Implementation',
    version: version,
    libraries: libcinnamon_menus,
    subdirs: 'cinnamon-menus-3.0'
)

gnome.generate_gir(
    libcinnamon_menus,
    namespace: 'CMenu',
    nsversion: '3.0',
    sources: public_sources,
    identifier_prefix: 'GMenu',
    symbol_prefix: 'gmenu',
    includes: 'Gio-2.0',
    header: 'gmenu-tree.h',
    install: true,
    install_dir_gir: join_paths(datadir, 'gir-1.0'),
    export_packages: 'libcinnamon-menu-3.0',
)
