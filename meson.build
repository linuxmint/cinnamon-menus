project('cinnamon-menus', 'c', version : '4.0.0')

gnome = import('gnome')

version = meson.project_version()
version_list = version.split('.')
major_version = version_list[0]
minor_version = version_list[1]
micro_version = version_list[2]

binary_version = '0.0.1'
binary_major_version = binary_version.split('.')[0]

xreader_conf = configuration_data()
xreader_conf.set_quoted('PACKAGE', meson.project_name())

# directories
prefix = get_option('prefix')
datadir = get_option('datadir')
libdir = get_option('libdir')
includedir = get_option('includedir')

# generate config.h
config_h_file = configure_file(
    output : 'config.h',
    configuration : xreader_conf
)

config_h = declare_dependency(
    sources: config_h_file
)

include_root = include_directories('.')

c_args = [
    '-DGMENU_I_KNOW_THIS_IS_UNSTABLE',
]

add_global_arguments(c_args, language: 'c')

gio = dependency('gio-unix-2.0', version: '>= 2.29.15')

subdir('libmenu')
subdir('docs/reference')
