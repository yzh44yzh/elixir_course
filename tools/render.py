import os
import glob  # https://docs.python.org/3/library/glob.html
import chevron  # https://github.com/noahmorrison/chevron


def render_all_lessons():
    for i in range(1, 15):
        lesson = 'lesson_{:02d}'.format(i)
        render_lesson(lesson)


def render_lesson(lesson):
    print('Render {}'.format(lesson))
    # tmp_path = './{}/tmp/*.md'.format(lesson) # TEMP
    tmp_path = './{}/tmp/*buzz.ru.md'.format(lesson)
    for template in glob.glob(tmp_path):
        render_template(template, lesson)


def render_template(template, lesson):
    print('  {}'.format(template))
    lib_dir = os.path.join(lesson, 'lib')
    with open(template, 'r') as t:
        content = chevron.render(t, partials_path=lib_dir, partials_ext='exs')
        basename = os.path.basename(template)
        out_file = os.path.join(lesson, basename)
        with open(out_file, 'w') as out:
            out.write(content)
    exit(0)  # TEMP


def insert_source(source, source_dir):
    s = source.split(':')
    source_file = '{}.exs'.format(s[0])
    mark = s[1]
    # TODO open file, find mark, get part of file
    return '```SOURCE from {} {} {}```'.format(source_dir, source_file, mark)


def try_chevron_lambda():
    template = 'insert part of file {{# source}}fizz_buzz_01:test{{/ source}} END'
    data = {
        'source': lambda source, _render: insert_source(source, 'lesson_01/lib')
    }
    res = chevron.render(template, data=data)
    print(res)


if __name__ == '__main__':
    # render_all_lessons()
    try_chevron_lambda()
