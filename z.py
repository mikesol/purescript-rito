A = '''color :: Color
      , roughness :: Number
      , metalness :: Number
      , map :: Texture
      , lightMap :: Texture
      , lightMapIntensity :: Number
      , aoMap :: Texture
      , aoMapIntensity :: Number
      , emissive :: Color
      , emissiveIntensity :: Number
      , emissiveMap :: Texture
      , bumpMap :: Texture
      , bumpScale :: Number
      , normalMap :: Texture
      , normalMapType :: NormalMapType
      , normalScale :: Vector2
      , displacementMap :: Texture
      , displacementScale :: Number
      , displacementBias :: Number
      , roughnessMap :: Texture
      , metalnessMap :: Texture
      , alphaMap :: Texture
      , envMap :: Texture
      , envMapIntensity :: Number
      , wireframe :: Boolean
      , wireframeLinewidth :: Number
      , flatShading :: Boolean'''
A=[[x for x in y.split(' ') if x != ''][0] for y in A.replace(',', ' ').split('\n')]
for x in A:
    cp = x[0].capitalize()+x[1:]
    print(f'''export const set{cp}_ = (a) => (state) => () => {{
	state.units[a.id].main.{x} = a.{x};
}};''')
