project('cinnamon-menus', 'c', version : '4.8.0', meson_version: '>=0.40.0')

gnome = import('gnome')

version = meson.project_version()

binary_version = '0.0.1'
binary_major_version = binary_version.split('.')[0]

cmenu_conf = configuration_data()
cmenu_conf.set_quoted('PACKAGE', meson.project_name())

# directories
prefix = get_option('prefix')
datadir = get_option('datadir')
libdir = get_option('libdir')
includedir = get_option('includedir')

# generate config.h
config_h_file = configure_file(
    output : 'config.h',
    configuration : cmenu_conf
)

config_h = declare_dependency(
    sources: config_h_file
)

include_root = include_directories('.')

c_args = [
    '-DGMENU_I_KNOW_THIS_IS_UNSTABLE',
]

if get_option('enable_debug')
    c_args += '-DG_ENABLE_DEBUG'
else
    c_args += '-DG_DISABLE_ASSERT'
    c_args += '-DG_DISABLE_CHECKS'
    c_args += '-DG_DISABLE_CAST_CHECKS'
endif

if not get_option('deprecated_warnings')
    c_args += '-Wno-deprecated-declarations'
    c_args += '-Wno-deprecated'
    c_args += '-Wno-declaration-after-statement'
endif

add_global_arguments(c_args, language: 'c')

gio = dependency('gio-unix-2.0', version: '>= 2.29.15')

subdir('libmenu')
if get_option('enable_docs')
subdir('docs/reference')
endif
