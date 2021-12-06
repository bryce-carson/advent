def pos(string):
  '''
  String is the Elf submarine command.
  Aim is the present aim, passed by reference.
  Poisition is the list: [horizontal, depth]; passed by reference also.
  '''
  
  str = string.split()
  
  global aim
  global position
  
  if str[0][0] != 'f':
    if str[0][0] == 'd':
      print("Command is not: `forward`.")
      print("Aim before command:", aim)
      aim += int(str[1])  # Increment aim, increasing depth.
      print("Aim after command:", aim)
    else:
      aim -= int(str[1])  # Decrement aim, decreasing depth.
  else:
    print("Command is: `forward`.")
    print("Position before command:", position)
    position = [position[0] + int(str[1]),
                position[1] + int(str[1]) * aim]
    print("Position after command:", position)
  
  # Void: aim, position.

with open("/Users/bryce/Downloads/inputDay2.txt") as d:
  data = d.readlines()

aim = 0
position = [0,0]

for command in data:
  pos(command)

print(position[0] * position[1])
