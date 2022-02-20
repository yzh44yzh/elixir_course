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


if __name__ == '__main__':
    render_all_lessons()
