# This is a sample commands.py.  You can add your own commands here.
#
# Please refer to commands_full.py for all the default commands and a complete
# documentation.  Do NOT add them all here, or you may end up with defunct
# commands when upgrading ranger.

# A simple command for demonstration purposes follows.
# -----------------------------------------------------------------------------

from __future__ import (absolute_import, division, print_function)

# You can import any python module as needed.
import os
import platform
import subprocess
from ranger.container.file import File
from ranger.ext.get_executables import get_executables
from command_functions import *

# You always need to import ranger.api.commands here to get the Command class:
from ranger.api.commands import Command


# Any class that is a subclass of "Command" will be integrated into ranger as a
# command.  Try typing ":my_edit<ENTER>" in ranger!
class my_edit(Command):
    # The so-called doc-string of the class will be visible in the built-in
    # help that is accessible by typing "?c" inside ranger.
    """:my_edit <filename>

    A sample command for demonstration purposes that opens a file in an editor.
    """

    # The execute method is called when you run this command in ranger.
    def execute(self):
        # self.arg(1) is the first (space-separated) argument to the function.
        # This way you can write ":my_edit somefilename<ENTER>".
        if self.arg(1):
            # self.rest(1) contains self.arg(1) and everything that follows
            target_filename = self.rest(1)
        else:
            # self.fm is a ranger.core.filemanager.FileManager object and gives
            # you access to internals of ranger.
            # self.fm.thisfile is a ranger.container.file.File object and is a
            # reference to the currently selected file.
            target_filename = self.fm.thisfile.path

        # This is a generic function to print text in ranger.
        self.fm.notify("Let's edit the file " + target_filename + "!")

        # Using bad=True in fm.notify allows you to print error messages:
        if not os.path.exists(target_filename):
            self.fm.notify("The given file does not exist!", bad=True)
            return

        # This executes a function from ranger.core.acitons, a module with a
        # variety of subroutines that can help you construct commands.
        # Check out the source, or run "pydoc ranger.core.actions" for a list.
        self.fm.edit_file(target_filename)

    # The tab method is called when you press tab, and should return a list of
    # suggestions that the user will tab through.
    # tabnum is 1 for <TAB> and -1 for <S-TAB> by default
    def tab(self, tabnum):
        # This is a generic tab-completion function that iterates through the
        # content of the current directory.
        return self._tab_directory_content()


def is_macos():
    return platform.system() == "Darwin"


def is_linux():
    return platform.system() == "Linux"


class fzf_select(Command):
    """
    :fzf_select
    Find a file using fzf.
    With a prefix argument to select only directories.

    See: https://github.com/junegunn/fzf
    """

    def execute(self):
        import subprocess
        import os
        from ranger.ext.get_executables import get_executables

        if 'fzf' not in get_executables():
            self.fm.notify('Could not find fzf in the PATH.', bad=True)
            return

        fd = None
        if 'fdfind' in get_executables():
            fd = 'fdfind'
        elif 'fd' in get_executables():
            fd = 'fd'

        if fd is not None:
            hidden = ('--hidden' if self.fm.settings.show_hidden else '')
            exclude = "--no-ignore-vcs --exclude '.git' --exclude '*.py[co]' --exclude '__pycache__'"
            only_directories = ('--type directory' if self.quantifier else '')
            fzf_default_command = '{} --follow {} {} {} --color=always'.format(
                fd, hidden, exclude, only_directories
            )
        else:
            hidden = (
                '-false' if self.fm.settings.show_hidden else r"-path '*/\.*' -prune")
            exclude = r"\( -name '\.git' -o -name '*.py[co]' -o -fstype 'dev' -o -fstype 'proc' \) -prune"
            only_directories = ('-type d' if self.quantifier else '')
            fzf_default_command = 'find -L . -mindepth 1 {} -o {} -o {} -print | cut -b3-'.format(
                hidden, exclude, only_directories
            )

        env = os.environ.copy()
        env['FZF_DEFAULT_COMMAND'] = fzf_default_command
        env[
            'FZF_DEFAULT_OPTS'] = '--height=40% --layout=reverse --ansi --preview="{}"'.format('''
            (
                batcat --color=always {} ||
                bat --color=always {} ||
                cat {} ||
                tree -ahpCL 3 -I '.git' -I '*.py[co]' -I '__pycache__' {}
            ) 2>/dev/null | head -n 100
        ''')

        fzf = self.fm.execute_command('fzf --no-multi', env=env,
                                      universal_newlines=True,
                                      stdout=subprocess.PIPE)
        stdout, _ = fzf.communicate()
        if fzf.returncode == 0:
            selected = os.path.abspath(stdout.strip())
            if os.path.isdir(selected):
                self.fm.cd(selected)
            else:
                self.fm.select_file(selected)


class show_current_dir_in_files(Command):
    def execute(self):
        if is_macos():
            self.fm.run('open .', flags='f')
        else:
            self.fm.run('nautilus .', flags='f')


class show_current_file_in_files(Command):
    def execute(self):
        if is_macos():
            self.fm.run('open -R ' + self.fm.thisfile.path, flags='f')
        else:
            self.fm.run('nautilus ' + self.fm.thisfile.path, flags='f')


class open_in_intellij(Command):
    def execute(self):
        if is_macos():
            self.fm.run(
                'idea "' + self.fm.thisfile.path + '" > /dev/null 2>&1 &',
                flags='f')
        else:
            self.fm.run(
                'intellij-idea-ultimate "' + self.fm.thisfile.path + '" > /dev/null 2>&1 &',
                flags='f')


class open_in_vscode(Command):
    def execute(self):
        self.fm.run('code "' + self.fm.thisfile.path + '" > /dev/null 2>&1 &',
                    flags='f')


class open_in_nvim_tab(Command):
    def execute(self):
        if is_macos():
            self.fm.run('alacritty --command nvim ' + self.fm.thisfile.path,
                        flags='f')
            # subprocess.Popen(["nvim", self.fm.thisfile.path])
        else:
            self.fm.run('alacritty -e nvim ' + self.fm.thisfile.path, flags='f')


class open_in_sublime(Command):
    def execute(self):
        self.fm.run('subl "' + self.fm.thisfile.path + '"', flags='f')


class open_terminal(Command):
    def execute(self):
        self.fm.run('alacritty', flags='f')


class copy_file_content_to_clipboard(Command):
    def execute(self):
        if is_macos():
            copy_file_content_to_clipboard_macos_fn(self)
        else:
            copy_file_content_to_clipboard_linux_fn(self)

    def tab(self, tabnum):
        return self._tab_directory_content()


class copy_file_to_clipboard(Command):
    def execute(self):
        if is_macos():
            copy_file_to_clipboard_macos_fn(self)
        else:
            # no good alternative to copy file in linux, as in mac, copy just content
            copy_file_content_to_clipboard_linux_fn(self)


class delete_highlighted(Command):
    """TODO:"""

    def execute(self):
        # highlighted_files = self.fm.thisdir.marked_items
        # for file in highlighted_files:
        # print(file.path)
        # self.fm.run('rm ' + file.path, flags='f')
        highlighted_files = self.fm.thisdir.get_selection()
        if not highlighted_files:
            self.fm.notify("No files are highlighted.", bad=True)
            return
        else:
            self.fm.notify("Highlighted files:", bad=False)

            items = "++"
            for file in highlighted_files:
                # self.fm.notify(file.relative_path, bad=False)
                # print(file.relative_path)
                items = items + " " + file.relative_path
                # self.fm.run('rm ' + file.relative_path, flags='f')
            print("---" + items)
