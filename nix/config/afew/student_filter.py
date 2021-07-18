from afew.filters.BaseFilter  import Filter
from afew.FilterRegistry import register_filter

import csv

# Each row of the csv should be: email, tag, program, tb

@register_filter
class StudentFilter(Filter):
  message = 'Organize student emails'

  def __init__(self, database):
    super().__init__(database)

    self.students = {}
    with open('/home/tom/studentlist.csv') as csvfile:
      reader = csv.DictReader(csvfile)
      for row in reader:
        self.students[row['email']] = row
    # TODO: maybe make this more specific?
    self.query = (
      '(' +
      ' OR '.join('"%s"' % addr for addr in self.students) +
      ')'
    )

  def handle_message(self, message):
    for addr in self.students:
      for header in ['To', 'From', 'Cc', 'Bcc']:
        if addr in message.get_header(header):
          subtag = 'students/%s' % (self.students[addr]['tag'])
          prog = self.students[addr]['program']
          tb = self.students[addr]['tb']
          self.add_tags(message, 'students', subtag, prog, tb)

