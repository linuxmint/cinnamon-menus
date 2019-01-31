public_headers = [
	'gmenu-tree.h',
]

public_sources = [
    'gmenu-tree.c',
    public_headers,
]

libmenu_sources = [
    'desktop-entries.c',
    'desktop-entries.h',
    'entry-directories.c',
    'entry-directories.h',
    'menu-layout.c',
    'menu-layout.h',
    'menu-monitor.c',
    'menu-monitor.h',
    'menu-util.c',
    'menu-util.h',
    public_sources,
]

libcinnamon_menus = library(
    'cinnamon-menu-3',
    libmenu_sources,
    soversion: binary_major_version,
    version: binary_version,
    include_directories: include_root,
    dependencies: [gio, config_h],
    install: true,
    build_by_default: false,
)

install_headers(
    public_headers,
    subdir: 'cinnamon-menus-3.0'
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
    identifier_prefix: 'GMenu',
    symbol_prefix: 'gmenu',
    includes: 'Gio-2.0',
    header: 'gmenu-tree.h',
    install: true,
    install_dir_gir: join_paths(datadir, 'gir-1.0'),
    export_packages: 'libcinnamon-menu-3.0',
)
