# Laboratory Work 1

## Description

Создать приложение на Haskell, используя classes и instances. В качестве БД использовать MySQL.

### Variant 7

Разработать информационную систему "Спорт на факультете".

Таблицы БД содержат такую информацию: Информация о студентах и преподавателях (....), которые занимаются в спортивных секциях, график работы секций, план проведения соревнований.

Предусмотреть возможность введения и корректирования информации.

Количество таблиц должно быть больше 5.

## Models

### Person

- `id`
- `first_name`
- `last_name`
- `position`    (**Student**, **Teacher**)

### SectionParticipant

- `section_id`
- `person_id`

### Section

- `id`
- `title`

### SectionSchedule

- `id`
- `section_id`
- `day_of_week`     (**0 Monday - 6 Sunday**)
- `time_start`      (**hh:mm**)
- `time_end`        (**hh:mm**)

### CompetitionParticipant

- `person_id`
- `competition_plan_id`
- `competition_result`

### CompetitionPlan

- `id`
- `competition_id`
- `winner_id`
- `date`

### Competition

- `id`
- `section_id`
- `title`