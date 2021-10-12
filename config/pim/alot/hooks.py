# many of these are from https://github.com/pazz/alot/wiki/Contrib-Hooks

import alot
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

##############################################################################
# 
def pre_buffer_open(ui, dbm, buf):
  current = ui.current_buffer
  if isinstance(current, alot.buffers.SearchBuffer):
    current.focused_thread = current.get_selected_thread()   # remember focus

def post_buffer_focus(ui, dbm, buf, success):
  if success and hasattr(buf, "focused_thread"):  # if buffer has saved focus
    if buf.focused_thread is not None:
      tid = buf.focused_thread.get_thread_id() 
      for pos, tlw in enumerate(buf.threadlist.get_lines()):
        if tlw.get_thread().get_thread_id() == tid:
          buf.body.set_focus(pos)
          break
