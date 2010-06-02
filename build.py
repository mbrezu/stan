#!/usr/bin/env python

import sys
from memoize import memoize, set_force_build

def run(cmd):
    status = memoize(cmd)
    if status: sys.exit(status)

def forceRun(cmd):
    set_force_build(True)
    status = memoize(cmd)
    set_force_build(False)
    if status: sys.exit(status)

base_modules = [
    "parserTypes",
    "utils",
    "pwm",
    "lexer",
    "plsqlParser",
    "absint",
    "acm",
    "absintCompiler",
    "experimental_absint"
]

tests_modules = [
    "testsCommon",
    "lexerTests",
    "pwmTests",
    "exprTests",
    "selectTests",
    "stmtTests",
    "absintTests",
    "absintCompilerTests",
    "tests",
]

speed_modules = ["speed",
                 ]

packages = ["oUnit"]

def compile_run(compiler, object_extension, modules, executable):
    global packages
    package_list = ",".join(packages)
    run("mkdir -p bin/tests")
    for module in modules:
        run("ocamlfind %s -c -thread -package %s -I src src/%s.ml" % (compiler,
                                                                      package_list,
                                                                      module) )
    object_list = " ".join(["src/%s.%s" % (module, object_extension)
                            for module in modules])
    run("ocamlfind %s -thread -o %s -linkpkg -package %s %s" % (compiler,
                                                                executable,
                                                                package_list,
                                                                object_list))
    forceRun(executable)

def tests(compiler, object_extension):
    global base_modules, tests_modules
    compile_run(compiler,
                object_extension,
                base_modules + tests_modules,
                "bin/tests/test")

def speed(compiler, object_extension):
    global base_modules, speed_modules
    compile_run(compiler,
                object_extension,
                base_modules + speed_modules,
                "bin/tests/speed")

def clean():
    forceRun("rm -rf bin")
    forceRun("rm src/*.cmo src/*.cmi src/*.o src/*.cmx")

if len(sys.argv) < 2:
    print "usage: ./build.py clean"
    print "usage: ./build.py tests"
    print "usage: ./build.py testsopt"
elif sys.argv[1] == "clean":
    clean()
elif sys.argv[1] == "tests":
    tests("ocamlc", "cmo")
elif sys.argv[1] == "testsopt":
    tests("ocamlopt", "cmx")
elif sys.argv[1] == "speed":
    speed("ocamlc", "cmo")
elif sys.argv[1] == "speedopt":
    speed("ocamlopt", "cmx")
else:
    print "unknown command " + sys.argv[1]
