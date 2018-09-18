
libcinnamon_menu_sources = [
  'canonicalize.c',
  'desktop-entries.c',
  'entry-directories.c',
  'gmenu-tree.c',
  'menu-layout.c',
  'menu-monitor.c',
  'menu-util.c',
]

libcinnamon_menu_headers = [
  'gmenu-tree.h',
]


libcinnamon_menu = library('cinnamon-menu-3',
  libcinnamon_menu_sources,
  include_directories: rootInclude,
  dependencies: [
    gio_unix,
  ],
  c_args: [
    '-DGMENU_I_KNOW_THIS_IS_UNSTABLE',
  ],
  install: true,
)

install_headers(
  libcinnamon_menu_headers,
  subdir: 'cinnamon-menus-3.0',
)

gnome.generate_gir(libcinnamon_menu,
  namespace: 'CMenu',
  nsversion: '3.0',
  identifier_prefix: 'GMenu',
  symbol_prefix: 'gmenu',
  sources: [ libcinnamon_menu_headers, 'gmenu-tree.c', ],
  extra_args: [
    '-DGMENU_I_KNOW_THIS_IS_UNSTABLE',
  ],
  install: true,
)

pkgconfig.generate(
  name: 'libcinnamon-menu',
  version: meson.project_version(),
  description: 'Desktop Menu Specification Implementation',
  requires: gio_unix,
  libraries: [ libcinnamon_menu, ],
  subdirs: 'cinnamon-menus-3.0',
)
