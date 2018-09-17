
project('cinnamon-menus',
  'c',
  version: '3.8.2',
  meson_version: '>=0.47.0',
)

pkgconfig = import('pkgconfig')
gnome = import('gnome')


###############################################################################
# Project configuration

config = configuration_data()
config.set_quoted('PACKAGE', meson.project_name())

config.set('G_ENABLE_DEBUG', get_option('buildtype').contains('debug'))

###############################################################################
# Dependencies

gio_unix = dependency('gio-unix-2.0', version: '>=2.29.15')


rootInclude = include_directories('.')
configure_file(
  output: 'config.h',
  configuration: config,
)

subdir('libmenu')
