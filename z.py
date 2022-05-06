A = '''  , rotationFromAxisAngle :: { axis :: Vector3, angle :: Number }
  , rotationFromEuler :: Euler
  , rotationFromMatrix :: Matrix4
  , rotationFromQuaternion :: Quaternion
  , rotateOnAxis :: { axis :: Vector3, angle :: Number }
  , rotateOnWorldAxis :: { axis :: Vector3, angle :: Number }
  , translateOnAxis :: { axis :: Vector3, distance :: Number }
  , translateX :: Number
  , translateY :: Number
  , translateZ :: Number'''
A=[[x for x in y.split(' ') if x != ''][0] for y in A.replace(',', ' ').split('\n')]
for x in A:
    cp = x[0].capitalize()+x[1:]
    print(f'''foreign import set{cp}_ :: Core.Set{cp}_ -> FFIThreeSnapshot -> Effect Unit''')
