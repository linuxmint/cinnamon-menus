version_conf = configuration_data()
version_conf.set('VERSION', version)

configure_file(
    input: 'version.xml.in',
    output: 'version.xml',
    configuration: version_conf,
)

gnome.gtkdoc(
    'cmenu',
    src_dir: join_paths(meson.source_root(), 'libmenu'),
    main_xml: 'cmenu-docs.xml',
    scan_args: ['--rebuild-types'],
    ignore_headers: libmenu_private_headers,
    dependencies: cmenu_dep,
    install: true,
)
