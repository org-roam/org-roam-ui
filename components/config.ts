import { Easing } from '@tweenjs/tween.js'
const options: string[] = []
const algorithms: { [name: string]: (percent: number) => number } = {}
for (let type in Easing) {
  for (let mode in (Easing as any)[type]) {
    let name = type + mode
    if (name === 'LinearNone') {
      name = 'Linear'
    }
    options.push(name)
    algorithms[name] = (Easing as any)[type][mode]
  }
}

export const algos = algorithms

export const initialPhysics = {
  enabled: true,
  charge: -700,
  collision: true,
  collisionStrength: 20,
  centering: true,
  centeringStrength: 0.2,
  linkStrength: 0.3,
  linkIts: 1,
  alphaDecay: 0.05,
  alphaTarget: 0,
  alphaMin: 0,
  velocityDecay: 0.25,
  gravity: 0.3,
  gravityOn: true,
  gravityLocal: false,
}

export const initialFilter = {
  orphans: false,
  dailies: false,
  parent: 'heading',
  filelessCites: false,
  tagsBlacklist: [],
  tagsWhitelist: [],
  bad: true,
  nodes: [],
  links: [],
  date: [],
}

export const initialVisuals = {
  particles: false,
  particlesNumber: 0,
  particlesWidth: 4,
  arrows: false,
  arrowsLength: 1,
  arrowsPos: 0.5,
  arrowsColor: '',
  linkOpacity: 0.8,
  linkWidth: 1,
  nodeRel: 4,
  nodeOpacity: 1,
  nodeResolution: 12,
  labels: 2,
  labelScale: 1.5,
  labelFontSize: 13,
  labelLength: 40,
  labelWordWrap: 25,
  labelLineSpace: 1,
  highlight: true,
  highlightNodeSize: 2,
  highlightLinkSize: 2,
  highlightFade: 0.8,
  highlightAnim: true,
  animationSpeed: 420,
  algorithmOptions: options,
  algorithmName: 'SinusoidalOut',
  linkColorScheme: 'gray.500',
  nodeColorScheme: [
    'red.500',
    'gray.600',
    'yellow.500',
    'green.500',
    'cyan.500',
    'blue.500',
    'pink.500',
    'purple.500',
    'orange.500',
  ],
  nodeHighlight: 'purple.500',
  linkHighlight: 'purple.500',
  backgroundColor: 'white',
  emacsNodeColor: 'gray.800',
  labelTextColor: 'gray.900',
  labelBackgroundColor: '',
  labelBackgroundOpacity: 0.7,
  citeDashes: true,
  citeDashLength: 35,
  citeGapLength: 15,
  citeLinkColor: 'gray.700',
  citeLinkHighlightColor: '',
  citeNodeColor: 'black',
  refDashes: true,
  refDashLength: 35,
  refGapLength: 15,
  refLinkColor: 'gray.700',
  refLinkHighlightColor: '',
  refNodeColor: 'black',
  nodeSizeLinks: 0.5,
  nodeZoomSize: 1.3,
}

export interface TagColors {
  [tag: string]: string
}

export const initialBehavior = {
  follow: 'zoom',
  localSame: 'add',
  zoomPadding: 200,
  zoomSpeed: 2000,
}

export const initialMouse = {
  highlight: 'hover',
  local: 'click',
  follow: 'double',
  context: 'right',
}

export const colorList = [
  'red.500',
  'orange.500',
  'yellow.500',
  'green.500',
  'cyan.500',
  'blue.500',
  'pink.500',
  'purple.500',
  'white',
  'gray.100',
  'gray.200',
  'gray.300',
  'gray.400',
  'gray.500',
  'gray.600',
  'gray.700',
  'gray.800',
  'gray.900',
  'black',
]
