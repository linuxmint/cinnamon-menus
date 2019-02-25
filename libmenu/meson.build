header_subdir = 'cinnamon-menus-3.0'

public_headers = [
    'cmenu-tree.h',
]

public_sources = [
    'cmenu-tree.c',
    public_headers,
]

public_sources += gnome.mkenums_simple(
    'cmenu-tree-enums',
    sources: public_headers,
    install_headers: true,
    install_dir: header_subdir,
    identifier_prefix: 'CMenu',
    symbol_prefix: 'cmenu',
)

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
    subdir: header_subdir,
)

pc_conf = configuration_data()
pc_conf.set('prefix', prefix)
pc_conf.set('exec_prefix', '${prefix}')
pc_conf.set('libdir', join_paths('${prefix}', libdir))
pc_conf.set('includedir', join_paths('${prefix}', includedir))
pc_conf.set('VERSION', version)

pc = configure_file(
    input: 'libcinnamon-menu-3.0.pc.in',
    output: 'libcinnamon-menu-3.0.pc',
    configuration: pc_conf,
    install: true,
    install_dir: join_paths(libdir, 'pkgconfig'),
)

libdocument_gir = gnome.generate_gir(
    libcinnamon_menus,
    namespace: 'CMenu',
    nsversion: '3.0',
    sources: public_sources,
    identifier_prefix: 'CMenu',
    symbol_prefix: 'cmenu',
    includes: 'Gio-2.0',
    header: 'cmenu-tree.h',
    install: true,
    install_dir_gir: join_paths(datadir, 'gir-1.0'),
    export_packages: 'libcinnamon-menu-3.0',
)
