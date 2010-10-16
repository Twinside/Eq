import sys
import re

import gtk
import gobject

from plugins import GajimPlugin
from plugins.helpers import log, log_calls

# For lib Eq
from ctypes import *
lib = cdll.LoadLibrary("eqlinlib.so")

class EqMathPlugin(GajimPlugin):

	@log_calls('EqMathPlugin')
	def init(self):
		self.config_dialog = None
		self.gui_extension_points = {
			'chat_control_base' : (self.connect_with_chat_control_base,
						self.disconnect_from_chat_control_base)
		}

	@log_calls('EqMathPlugin')
	def textbuffer_live_eqmath(self, tb):
		t = tb.get_text(tb.get_start_iter(), tb.get_end_iter())
		#if t and t[-1] == INVOKER:
		#	base, sep, head = t[:-1].rpartition(INVOKER)
		#	log.debug('%s | %s | %s' % (base, sep, head))
		#	if head == 'prout':
		#		t = ''.join((base, sep, 'plouarf', INVOKER))
		#		gobject.idle_add(tb.set_text, t)
		if(t.find('(eq:') != -1):
			values = self.replace_by_eq(t)
			print 'return values'
			print values[0]
			print values[1]
			if(values[0] == 1):
				gobject.idle_add(tb.set_text, values[1])
	

	@log_calls('EqMathPlugin')
	def connect_with_chat_control_base(self, chat_control):
		d = {}
		tv = chat_control.msg_textview
		tb = tv.get_buffer()
		h_id = tb.connect('changed', self.textbuffer_live_eqmath)
		d['h_id'] = h_id

		chat_control.eqmath_plugin_data = d

		return True

	@log_calls('EqMathPlugin')
	def disconnect_from_chat_control_base(self, chat_control):
		d = chat_control.eqmath_plugin_data
		tv = chat_control.msg_textview
		tv.get_buffer().disconnect(d['h_id'])

	@log_calls('EqMathPlugin')
	def replace_by_eq(self, origin_str):
		changed = 0
		return_str = origin_str
		index_of_eq = origin_str.find('(eq:')
		if (index_of_eq != -1):
			tmp_str = ""
			counter = 1 #Count the first mark !
			for c in origin_str[index_of_eq + 4:]:
				if ( c == '('):
					counter = counter + 1
				if ( c == ')'):
					counter = counter - 1
					if (counter == 0):
						break
				tmp_str += c
			if (counter == 0):
				length_to_replace = len(tmp_str) + index_of_eq + 7
				tmp_str = c_char_p(lib.eqEval(tmp_str)).value
				return_str = origin_str[0:index_of_eq] + '\n' + tmp_str + '\n' + origin_str[length_to_replace:]
				changed = 1
				#return_str = self.replace_by_eq(return_str)
		print 'eqmathcall'
		print return_str
		print 'changed : %d' % changed
		return (changed,return_str)

