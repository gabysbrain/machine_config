# many of these are from https://github.com/pazz/alot/wiki/Contrib-Hooks

import re

##############################################################################
# Override the from address lookup so my work address stays where it belongs
transitions = [
    ('.*swansea.ac.uk.*', 'Thomas Torsney-Weir <t.d.torsney-weir@swansea.ac.uk>')
  ]
addr_trans = []
for addr, fr in transitions:
  addr_trans.append((re.compile("(To|Cc): %s" % addr, re.MULTILINE), 
                     "From: %s" % fr))

def pre_edit_translate(bodytext, ui, dbm):
  for addr, new_from in addr_trans:
    if addr.search(bodytext):
      return re.sub('^From: .*$', new_from, bodytext, flags=re.MULTILINE)
  return bodytext

##############################################################################
# Auto-refresh search buffer

def pre_buffer_focus(ui, dbm, buf):
  if buf.modename == 'search':
    buf.rebuild()

