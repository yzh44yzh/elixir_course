import os
import glob  # https://docs.python.org/3/library/glob.html
import chevron  # https://github.com/noahmorrison/chevron


def render_all_lessons():
    for i in range(1, 15):
        lesson = 'lesson_{:02d}'.format(i)
        render_lesson(lesson)


def render_lesson(lesson):
    print(f'Render {lesson}')
    tmp_path = f'./{lesson}/tmp/*.md'
    for template in glob.glob(tmp_path):
        render_template(template, lesson)


def render_template(template, lesson):
    print(f'  {template}')
    lib_dir = os.path.join(lesson, 'lib')
    with open(template, 'r') as t:
        content = t.read()
        data = {
            'source': lambda source, _render: insert_source(source, f'{lesson}/lib')
        }
        content = chevron.render(content, data=data)
        basename = os.path.basename(template)
        out_file = os.path.join(lesson, basename)
        with open(out_file, 'w') as out:
            out.write(content)


def insert_source(source, source_dir):
    s = source.split(':')
    source_file = os.path.join(source_dir, f'{s[0]}.exs')
    with open(source_file, 'r') as sf:
        source = sf.read()
        if len(s) > 1:
            begin_mark = f'# MARK:{s[1]}'
            begin_pos = source.find(begin_mark)
            if begin_pos == -1:
                raise RuntimeError(f'mark {s[1]} is not found in {source_file}')
            begin_pos += len(begin_mark)
            end_mark = f'# END_MARK:{s[1]}'
            end_pos = source.find(end_mark)
            if end_pos == -1:
                raise RuntimeError(f'end of mark {s[1]} is not found in {source_file}')
            return source[begin_pos:end_pos].strip()
        else:
            return source.strip()


if __name__ == '__main__':
    render_all_lessons()
