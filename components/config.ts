import { Easing } from '@tweenjs/tween.js'

export const initialPhysics = {
  enabled: true,
  charge: -350,
  collision: true,
  collisionStrength: 0,
  linkStrength: 0.1,
  linkIts: 1,
  particles: false,
  particlesNumber: 0,
  particlesWidth: 4,
  linkOpacity: 0.4,
  linkWidth: 1,
  nodeRel: 4,
  labels: true,
  labelScale: 1.5,
  alphaDecay: 0.02,
  alphaTarget: 0,
  alphaMin: 0,
  velocityDecay: 0.25,
  gravity: 0.5,
  gravityOn: true,
  colorful: true,
  galaxy: true,
  ticks: 1,
  hover: 'highlight',
  click: 'select',
  doubleClick: 'local',
  iterations: 0,
  highlight: true,
  highlightNodeSize: 2,
  highlightLinkSize: 2,
  highlightAnim: false,
  animationSpeed: 250,
  algorithms: getAlgos(false),
  algorithmOptions: getAlgos(true),
  algorithmName: 'CubicOut',
  orphans: false,
  follow: 'Local',
}

export const initialFilter = {
  orphans: false,
  parents: true,
  tags: [],
  nodes: [],
  links: [],
  date: [],
}

function getAlgos(option?: boolean) {
  const options: string[] = []
  const algorithms: { [name: string]: (percent: number) => number } = {}
  for (let type in Easing) {
    for (let mode in (Easing as any)[type]) {
      let name = type + mode
      if (name === 'LinearNone') {
        name = 'Linear'
      }
      option ? options.push(name) : (algorithms[name] = (Easing as any)[type][mode])
    }
  }
  return option ? options : algorithms
}
