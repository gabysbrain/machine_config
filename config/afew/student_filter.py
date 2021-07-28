from afew.filters.BaseFilter  import Filter
from afew.FilterRegistry import register_filter

import csv

# kwargs in constructor processes config

# Each row of the csv should be: email, tag, program, tb

@register_filter
class StudentFilter(Filter):
  message = 'Organize student emails'
  list_file = None

  def __init__(self, database, **kwargs):
    super().__init__(database, **kwargs)

    self.students = {}
    with open(self.list_file) as csvfile:
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

