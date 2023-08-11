# Copyright 2023 Theresa Swift and Carl Anderson
#
# Permission is hereby granted, free of charge,  to any person obtaining a
# copy  of  this  software  and    associated   documentation  files  (the
# “Software”), to deal in  the   Software  without  restriction, including
# without limitation the rights to  use,   copy,  modify,  merge, publish,
# distribute, sublicense, and/or sell  copies  of   the  Software,  and to
# permit persons to whom the Software is   furnished  to do so, subject to
# the following conditions:
#
# The above copyright notice and this  permission notice shall be included
# in all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT  WARRANTY OF ANY KIND, EXPRESS
# OR  IMPLIED,  INCLUDING  BUT  NOT   LIMITED    TO   THE   WARRANTIES  OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR   PURPOSE AND NONINFRINGEMENT.
# IN NO EVENT SHALL THE AUTHORS  OR   COPYRIGHT  HOLDERS BE LIABLE FOR ANY
# CLAIM, DAMAGES OR OTHER LIABILITY,  WHETHER   IN  AN ACTION OF CONTRACT,
# TORT OR OTHERWISE, ARISING FROM,  OUT  OF   OR  IN  CONNECTION  WITH THE
# SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

class Person:
  def __init__(self, name, age, favorite_ice_cream=None):
    self.name = name
    self.age = age
    if favorite_ice_cream is None:
      favorite_ice_cream = 'chocolate'
    self.favorite_ice_cream = favorite_ice_cream

  def func0(self):
    return("Hello my name is " + self.name)

  def func1(self,myclass):
    return("Hello my name is " + self.name + " and I'm a " + myclass)

  def func2(self,myclass1,myclass2):
    return("Hello my name is " + self.name + " and I'm a " + myclass1 + " " + myclass2)

  def func3(self,myclass1,myclass2,myclass3):
    return("Hello my name is " + self.name + " and I'm a " + myclass1 + " " + myclass2
           + " " + myclass3)

  def func4(self,myclass1,myclass2,myclass3,myclas4):
    return("Hello my name is " + self.name + " and I'm a " + myclass1 + " " + myclass2
           + " " + myclass3 + " I mean " + mylass4)


#p1 = Person("John", 36)
#p1.myfunc()
