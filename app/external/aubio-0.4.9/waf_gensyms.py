import re
import os.path
from waflib import TaskGen, Task
from waflib.Context import STDOUT
from waflib.Utils import O644

class gen_sym_file(Task.Task):
    color = 'BLUE'
    inst_to = '${LIBDIR}'
    def run(self):
        syms = {}
        reg = getattr(self.generator, 'export_symbols_regex','.+?')
        if 'msvc' in self.env.CC_NAME:
            outputs = [x.abspath() for x in self.generator.link_task.outputs]
            binary_path = list(filter(lambda x: x.endswith('lib'), outputs))[0]
            reg_compiled = re.compile(r'External\s+\|\s+(?P<symbol>%s)\b' % reg)
            cmd =(self.env.LINK_CC) + ['/dump', '/symbols', binary_path]
        else: # using gcc? assume we have nm
            outputs = [x.abspath() for x in self.generator.link_task.outputs]
            binary_path = list(filter(lambda x: x.endswith('dll'), outputs))[0]
            reg_compiled = re.compile(r'(T|D)\s+_(?P<symbol>%s)\b'%reg)
            cmd = (self.env.NM or ['nm']) + ['-g', binary_path]
        dump_output = self.generator.bld.cmd_and_log(cmd, quiet=STDOUT)
        syms = set([])
        for m in reg_compiled.finditer(dump_output):
            syms.add(m.group('symbol'))
        syms = list(syms)
        syms.sort()
        self.outputs[0].write('EXPORTS\n'+'\n'.join(syms))

@TaskGen.feature('gensyms')
@TaskGen.after_method('process_source','process_use','apply_link','process_uselib_local','propagate_uselib_vars')
def gen_symbols(self):
    #sym_file = self.path.find_or_declare(self.target + '.def')
    sym_file_name = os.path.splitext(self.link_task.outputs[0].abspath())[0] + '.def'
    sym_file = self.path.find_or_declare(sym_file_name)
    symtask = self.create_task('gen_sym_file', self.link_task.outputs, sym_file)
    self.add_install_files(install_to=self.link_task.inst_to, install_from=sym_file,
        chmod=O644, task=self.link_task)

