import React, { ComponentPropsWithoutRef, useEffect, useRef, useState, useMemo } from 'react'
import { usePersistantState } from '../util/persistant-state'
const d3promise = import('d3-force-3d')

import type {
  ForceGraph2D as TForceGraph2D,
  ForceGraph3D as TForceGraph3D,
} from 'react-force-graph'
import { OrgRoamGraphReponse, OrgRoamLink, OrgRoamNode } from '../api'
import { GraphData, NodeObject } from 'force-graph'

import { useWindowSize } from '@react-hook/window-size'
import { Scrollbars } from 'react-custom-scrollbars-2'
import { Easing } from '@tweenjs/tween.js'
import { useAnimation } from '@lilib/hooks'

import {
  Accordion,
  AccordionButton,
  AccordionItem,
  AccordionIcon,
  AccordionPanel,
  Text,
  Heading,
  VStack,
  StackDivider,
  Button,
  CloseButton,
  Slider,
  SliderThumb,
  SliderTrack,
  SliderFilledTrack,
  Switch,
  FormControl,
  FormLabel,
  Box,
  Container,
  Icon,
  IconButton,
  Tooltip,
  Menu,
  MenuList,
  MenuButton,
  MenuItem,
  MenuGroup,
  MenuDivider,
  MenuOptionGroup,
  MenuItemOption,
  Flex,
  useTheme,
  Select,
} from '@chakra-ui/react'

import { InfoOutlineIcon, RepeatClockIcon, ChevronDownIcon, SettingsIcon } from '@chakra-ui/icons'

// react-force-graph fails on import when server-rendered
// https://github.com/vasturiano/react-force-graph/issues/155
const ForceGraph2D = (
  !!global.window ? require('react-force-graph').ForceGraph2D : null
) as typeof TForceGraph2D

const ForceGraph3D = (
  !!global.window ? require('react-force-graph').ForceGraph3D : null
) as typeof TForceGraph3D

export type NodeById = { [nodeId: string]: OrgRoamNode | undefined }
export type LinksByNodeId = { [nodeId: string]: OrgRoamLink[] | undefined }
export type Scope = {
  nodeIds: string[]
}

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
const initialPhysics = {
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
  algorithms: algorithms,
  algorithmOptions: options,
  algorithmName: 'CubicOut',
  orphans: false,
}

export default function Home() {
  // only render on the client
  const [showPage, setShowPage] = useState(false)
  useEffect(() => {
    setShowPage(true)
  }, [])

  if (!showPage) {
    return null
  }

  return <GraphPage />
}

export function GraphPage() {
  const [physics, setPhysics] = usePersistantState('physics', initialPhysics)
  //  const [theme, setTheme] = useState(initialTheme)
  const [graphData, setGraphData] = useState<GraphData | null>(null)
  const [emacsNodeId, setEmacsNodeId] = useState<string | null>(null)

  const nodeByIdRef = useRef<NodeById>({})
  const linksByNodeIdRef = useRef<LinksByNodeId>({})

  const updateGraphData = () => {
    return fetch('http://localhost:35901/graph')
      .then((res) => res.json())
      .then((orgRoamGraphData: OrgRoamGraphReponse) => {
        nodeByIdRef.current = Object.fromEntries(
          orgRoamGraphData.nodes.map((node) => [node.id, node]),
        )
        linksByNodeIdRef.current = orgRoamGraphData.links.reduce<LinksByNodeId>((acc, link) => {
          return {
            ...acc,
            [link.source]: [...(acc[link.source] ?? []), link],
            [link.target]: [...(acc[link.target] ?? []), link],
          }
        }, {})

        // react-force-graph modifies the graph data implicitly,
        // so we make sure there's no overlap between the objects we pass it and
        // nodeByIdRef, linksByNodeIdRef
        const orgRoamGraphDataClone = JSON.parse(JSON.stringify(orgRoamGraphData))
        setGraphData(orgRoamGraphDataClone)
      })
  }

  useEffect(() => {
    const trackEmacs = new EventSource('http://127.0.0.1:35901/current-node-id')
    trackEmacs.addEventListener('message', (e) => {
      const emacsNodeId = e.data
      setEmacsNodeId(emacsNodeId)
    })
    updateGraphData()
  }, [])

  useEffect(() => {
    if (!emacsNodeId) {
      return
    }
    updateGraphData()
  }, [emacsNodeId])

  const [threeDim, setThreeDim] = useState(false)
  const [showTweaks, setShowTweaks] = useState(true)

  if (!graphData) {
    return null
  }

  return (
    <div>
      {showTweaks ? (
        <Tweaks
          {...{
            physics,
            setPhysics,
            threeDim,
          }}
          onClose={() => {
            setShowTweaks(false)
          }}
        />
      ) : (
        <Box position="absolute" zIndex="overlay" marginTop="2%" marginLeft="2%">
          <IconButton
            aria-label="Settings"
            icon={<SettingsIcon />}
            onClick={() => setShowTweaks(true)}
          />
        </Box>
      )}
      <Graph
        nodeById={nodeByIdRef.current!}
        linksByNodeId={linksByNodeIdRef.current!}
        {...{
          physics,
          graphData,
          threeDim,
          emacsNodeId,
        }}
      />
    </div>
  )
}

export interface InfoTooltipProps {
  infoText?: string | boolean
}
export const InfoTooltip = (props: InfoTooltipProps) => {
  const { infoText } = props
  return (
    <Box paddingLeft="1">
      <Tooltip label={infoText} placement="top" color="gray.100" bg="gray.800" hasArrow>
        <InfoOutlineIcon />
      </Tooltip>
    </Box>
  )
}
export interface SliderWithInfoProps {
  min?: number
  max?: number
  step?: number
  value: number
  onChange: (arg0: number) => void
  label: string
  infoText?: string
}
export const SliderWithInfo = ({
  min = 0,
  max = 10,
  step = 0.1,
  value = 1,
  ...rest
}: SliderWithInfoProps) => {
  const { onChange, label, infoText } = rest
  return (
    <Box>
      <Box display="flex" alignItems="flex-end">
        <Text>{label}</Text>
        {infoText && <InfoTooltip infoText={infoText} />}
      </Box>
      <Slider
        value={value}
        onChange={onChange}
        min={min}
        max={max}
        step={step}
        colorScheme="purple"
      >
        <SliderTrack>
          <SliderFilledTrack />
        </SliderTrack>
        <Tooltip label={value.toFixed(1)}>
          <SliderThumb />
        </Tooltip>
      </Slider>
    </Box>
  )
}

export interface EnableSectionProps {
  label: string
  value: boolean | number
  onChange: () => void
  infoText?: string
  children: React.ReactNode
}

export const EnableSection = (props: EnableSectionProps) => {
  const { value, onChange, label, infoText, children } = props
  return (
    <Box>
      <Box display="flex" justifyContent="space-between">
        <Box display="flex" alignItems="center">
          <Text>{label}</Text>
          {infoText && <InfoTooltip infoText={infoText} />}
        </Box>
        <Switch isChecked={!!value} onChange={onChange} colorScheme="purple" />
      </Box>
      {value && children}
    </Box>
  )
}

export interface DropDownMenuProps {
  textArray: string[]
  onClickArray: any
  displayValue: string
}

export const DropDownMenu = (props: DropDownMenuProps) => {
  const { textArray, onClickArray, displayValue } = props
  return (
    <Menu>
      <MenuButton as={Button} rightIcon={<ChevronDownIcon />}>
        {displayValue}
      </MenuButton>
      <MenuList>
        {textArray.map((option, i) => {
          ;<MenuItem onClick={onClickArray[i]}> {option} </MenuItem>
        })}
      </MenuList>
    </Menu>
  )
}
/* style={{
 *   position: "absolute",
 *   zIndex: 2000,
 *   width: 400,
 *   maxHeight: "70%",
 *   background: "alt.100",
 *   marginTop: "2%",
 *   marginLeft: "2%"
 * }} */
export interface TweakProps {
  physics: typeof initialPhysics
  setPhysics: any
  threeDim: boolean
  onClose: () => void
}
export const Tweaks = (props: TweakProps) => {
  const { physics, setPhysics, threeDim, onClose } = props
  return (
    <Box
      zIndex="overlay"
      position="absolute"
      bg="alt.100"
      w="xs"
      marginTop="2%"
      marginLeft="2%"
      borderRadius="md"
      maxH={650}
      paddingBottom={5}
      //overflowY="scroll"
    >
      <Box display="flex" justifyContent="flex-end">
        <Tooltip label="Reset settings to defaults">
          <IconButton
            aria-label="Reset Defaults"
            icon={<RepeatClockIcon />}
            onClick={() => setPhysics(initialPhysics)}
            colorScheme="purple"
          />
        </Tooltip>
        <CloseButton onClick={onClose} />
      </Box>
      <Scrollbars
        autoHeight
        autoHeightMax={600}
        autoHide
        renderThumbVertical={({ style, ...props }) => (
          <Box
            {...props}
            style={{
              ...style,
              borderRadius: 10,
            }}
            bg="purple.500"
          />
        )}
      >
        <Accordion allowMultiple allowToggle>
          <AccordionItem>
            <AccordionButton display="flex" justifyContent="space-between">
              <Box display="flex">
                <AccordionIcon />
                <Text>Physics</Text>
              </Box>
              <Switch
                id="physicsOn"
                onChange={() => setPhysics({ ...physics, enabled: !physics.enabled })}
                isChecked={physics.enabled}
                colorScheme="purple"
              />
            </AccordionButton>
            <AccordionPanel>
              <VStack
                spacing={2}
                justifyContent="flex-start"
                divider={<StackDivider borderColor="gray.200" />}
                align="stretch"
              >
                <EnableSection
                  label="Gravity"
                  value={physics.gravityOn}
                  onChange={() => setPhysics({ ...physics, gravityOn: !physics.gravityOn })}
                >
                  <SliderWithInfo
                    label="Strength"
                    value={physics.gravity * 10}
                    onChange={(v) => setPhysics({ ...physics, gravity: v / 10 })}
                  />
                </EnableSection>
                <SliderWithInfo
                  value={-physics.charge / 100}
                  onChange={(value) => setPhysics({ ...physics, charge: -100 * value })}
                  label="Repulsive Force"
                />
                <EnableSection
                  label="Collision"
                  infoText="Perfomance sap, disable if slow"
                  value={physics.collision}
                  onChange={() => setPhysics({ ...physics, collision: !physics.collision })}
                >
                  <SliderWithInfo
                    value={physics.collisionStrength * 10}
                    onChange={(value) => setPhysics({ ...physics, collisionStrength: value / 10 })}
                    label="Strength"
                  />
                </EnableSection>
                <SliderWithInfo
                  value={physics.linkStrength * 5}
                  onChange={(value) => setPhysics({ ...physics, linkStrength: value / 5 })}
                  label="Link Force"
                />
                <SliderWithInfo
                  label="Link Iterations"
                  value={physics.linkIts}
                  onChange={(value) => setPhysics({ ...physics, linkIts: value })}
                  min={0}
                  max={6}
                  step={1}
                  infoText="How many links down the line the physics of a single node affects (Slow)"
                />
                <SliderWithInfo
                  label="Viscosity"
                  value={physics.velocityDecay * 10}
                  onChange={(value) => setPhysics({ ...physics, velocityDecay: value / 10 })}
                />
              </VStack>
              <Box>
                <Accordion allowToggle>
                  <AccordionItem>
                    <AccordionButton>
                      <Text>Advanced</Text>
                      <AccordionIcon />
                    </AccordionButton>
                    <AccordionPanel>
                      <VStack
                        spacing={2}
                        justifyContent="flex-start"
                        divider={<StackDivider borderColor="gray.200" />}
                        align="stretch"
                      >
                        <SliderWithInfo
                          label="Iterations per tick"
                          min={1}
                          max={10}
                          step={1}
                          value={physics.iterations}
                          onChange={(v) => setPhysics({ ...physics, iterations: v })}
                          infoText="Number of times the physics simulation iterates per simulation step"
                        />
                        <SliderWithInfo
                          label="Stabilization rate"
                          value={physics.alphaDecay * 50}
                          onChange={(value) => setPhysics({ ...physics, alphaDecay: value / 50 })}
                        />
                      </VStack>
                    </AccordionPanel>
                  </AccordionItem>
                </Accordion>
              </Box>
              {/* </VStack> */}
            </AccordionPanel>
          </AccordionItem>
          <AccordionItem>
            <AccordionButton>
              <AccordionIcon />
              Visual
            </AccordionButton>
            <AccordionPanel>
              <VStack
                spacing={2}
                justifyContent="flex-start"
                divider={<StackDivider borderColor="gray.200" />}
                align="stretch"
              >
                {/* <Box>
                  <Text>Kill orphans</Text>
                  <Switch
                    colorScheme="purple"
                    onChange={() => {
                      setPhysics({ ...physics, orphans: !physics.orphans })
                    }}
                    isChecked={physics.orphans}
                  ></Switch>
                </Box> */}
                <SliderWithInfo
                  label="Node size"
                  value={physics.nodeRel}
                  onChange={(value) => setPhysics({ ...physics, nodeRel: value })}
                />
                <SliderWithInfo
                  label="Link width"
                  value={physics.linkWidth}
                  onChange={(value) => setPhysics({ ...physics, linkWidth: value })}
                />
                <EnableSection
                  label="Labels"
                  value={physics.labels}
                  onChange={() => setPhysics({ ...physics, labels: !physics.labels })}
                >
                  <SliderWithInfo
                    label="Label Appearance Scale"
                    value={physics.labelScale * 5}
                    onChange={(value) => setPhysics({ ...physics, labelScale: value / 5 })}
                  />
                </EnableSection>
                <EnableSection
                  label="Directional Particles"
                  value={physics.particles}
                  onChange={() => setPhysics({ ...physics, particles: !physics.particles })}
                >
                  <SliderWithInfo
                    label="Particle Number"
                    value={physics.particlesNumber}
                    max={5}
                    step={1}
                    onChange={(value) => setPhysics({ ...physics, particlesNumber: value })}
                  />
                  <SliderWithInfo
                    label="Particle Size"
                    value={physics.particlesWidth}
                    onChange={(value) => setPhysics({ ...physics, particlesWidth: value })}
                  />
                </EnableSection>
                <EnableSection
                  label="Highlight Animation"
                  onChange={() => {
                    setPhysics({ ...physics, highlightAnim: !physics.highlightAnim })
                  }}
                  value={physics.highlightAnim}
                >
                  <SliderWithInfo
                    label="Animation speed"
                    onChange={(v) => setPhysics({ ...physics, animationSpeed: v })}
                    value={physics.animationSpeed}
                    infoText="Slower speed has a chance of being buggy"
                    min={50}
                    max={1000}
                    step={10}
                  />
                  <Select
                    placeholder={physics.algorithmName}
                    onChange={(v) => setPhysics({ ...physics, algorithmName: v.target.value })}
                  >
                    {physics.algorithmOptions.map((opt, i) => (
                      <option key={i} value={physics.algorithmOptions[i]}>
                        {' '}
                        {physics.algorithmOptions[i]}
                      </option>
                    ))}
                  </Select>
                  {/* <DropDownMenu
                    displayValue={physics.algorithmName}
                    textArray={physics.algorithmOptions}
                    onClickArray={physics.algorithmOptions.map((option) =>
                      setPhysics({ ...physics, algorithmName: { option } }),
                    )}
                  /> */}
                </EnableSection>
                <EnableSection
                  label="Highlight"
                  onChange={() => setPhysics({ ...physics, highlight: !physics.highlight })}
                  value={physics.highlight}
                >
                  <SliderWithInfo
                    label="Highlight Link Thickness Multiplier"
                    value={physics.highlightLinkSize}
                    onChange={(value) => setPhysics({ ...physics, highlightLinkSize: value })}
                  />
                  <SliderWithInfo
                    label="Highlight Node Size Multiplier"
                    value={physics.highlightNodeSize}
                    onChange={(value) => setPhysics({ ...physics, highlightNodeSize: value })}
                  />
                  <Flex justifyContent="space-between">
                    <Text> Highlight node color </Text>
                  </Flex>
                  <Flex justifyContent="space-between">
                    <Text> Highlight link color </Text>
                  </Flex>
                </EnableSection>
              </VStack>
            </AccordionPanel>
          </AccordionItem>
          <AccordionItem>
            <AccordionButton>
              <AccordionIcon />
              Behavior
            </AccordionButton>
            <AccordionPanel>
              <VStack
                spacing={2}
                justifyContent="flex-start"
                divider={<StackDivider borderColor="gray.200" />}
                align="stretch"
              >
                <Box display="flex" justifyContent="space-between" alignItems="center">
                  <Text>Hover Higlight</Text>
                  <Menu>
                    <MenuButton as={Button} rightIcon={<ChevronDownIcon />}>
                      {physics.hover}
                    </MenuButton>
                    <MenuList>
                      <MenuItem>Off</MenuItem>
                      <MenuItem>On</MenuItem>
                    </MenuList>
                  </Menu>
                </Box>
                <Box display="flex" justifyContent="space-between">
                  <Text>Click</Text>
                </Box>
                <Box display="flex" justifyContent="space-between">
                  <Text>Double-click</Text>
                </Box>
              </VStack>
            </AccordionPanel>
          </AccordionItem>
        </Accordion>
      </Scrollbars>
    </Box>
  )
}

export interface GraphProps {
  nodeById: NodeById
  linksByNodeId: LinksByNodeId
  graphData: GraphData
  physics: typeof initialPhysics
  threeDim: boolean
  emacsNodeId: string | null
}

export const Graph = function (props: GraphProps) {
  const { physics, graphData, threeDim, linksByNodeId, emacsNodeId, nodeById } = props

  const graph2dRef = useRef<any>(null)
  const graph3dRef = useRef<any>(null)

  // react-force-graph does not track window size
  // https://github.com/vasturiano/react-force-graph/issues/233
  // does not work below a certain width
  const [windowWidth, windowHeight] = useWindowSize()

  const [hoverNode, setHoverNode] = useState<NodeObject | null>(null)
  const [scope, setScope] = useState<Scope>({ nodeIds: [] })

  useEffect(() => {
    if (!emacsNodeId) {
      return
    }

    setScope({
      nodeIds: [emacsNodeId],
    })
  }, [emacsNodeId])

  const centralHighlightedNode = hoverNode
  const highlightedNodes = (() => {
    if (!centralHighlightedNode) {
      return {}
    }

    const links = linksByNodeId[centralHighlightedNode.id!]
    if (!links) {
      return {}
    }

    return Object.fromEntries(
      [
        centralHighlightedNode.id! as string,
        ...links.flatMap((link) => [link.source, link.target]),
      ].map((nodeId) => [nodeId, {}]),
    )
  })()

  const scopedNodes = graphData.nodes.filter((node) => {
    const links = linksByNodeId[node.id as string] ?? []
    /* if (physics.orphans && links.length === 0) {
     *   return false
     * } */
    return (
      scope.nodeIds.includes(node.id as string) ||
      links.some((link) => {
        return scope.nodeIds.includes(link.source) || scope.nodeIds.includes(link.target)
      })
    )
  })

  const scopedNodeIds = scopedNodes.map((node) => node.id as string)

  const scopedLinks = graphData.links.filter((link) => {
    // we need to cover both because force-graph modifies the original data
    // but if we supply the original data on each render, the graph will re-render sporadically
    const sourceId = typeof link.source === 'object' ? link.source.id! : (link.source as string)
    const targetId = typeof link.target === 'object' ? link.target.id! : (link.target as string)

    return scopedNodeIds.includes(sourceId as string) && scopedNodeIds.includes(targetId as string)
  })

  const scopedGraphData = useMemo(
    () =>
      scope.nodeIds.length === 0
        ? graphData
        : {
            nodes: scopedNodes,
            links: scopedLinks,
          },
    [scope, JSON.stringify(Object.keys(nodeById))],
  )

  // make sure the camera position and zoom or fine when the list of nodes to render is changed
  useEffect(() => {
    // this setTimeout was added holistically because the behavior is better when we put
    // zoomToFit off a little bit
    setTimeout(() => {
      const fg = threeDim ? graph3dRef.current : graph2dRef.current
      fg?.zoomToFit(0, 200)
    }, 1)
  }, [JSON.stringify(scopedNodeIds)])

  useEffect(() => {
    ;(async () => {
      const fg = threeDim ? graph3dRef.current : graph2dRef.current
      const d3 = await d3promise
      if (physics.gravityOn) {
        fg.d3Force('x', d3.forceX().strength(physics.gravity))
        fg.d3Force('y', d3.forceY().strength(physics.gravity))
        if (threeDim) {
          if (physics.galaxy) {
            fg.d3Force('x', d3.forceX().strength(physics.gravity / 5))
            fg.d3Force('z', d3.forceZ().strength(physics.gravity / 5))
          } else {
            fg.d3Force('x', d3.forceX().strength(physics.gravity))
            fg.d3Force('z', d3.forceZ().strength(physics.gravity))
          }
        } else {
          fg.d3Force('z', null)
        }
      } else {
        fg.d3Force('x', null)
        fg.d3Force('y', null)
        threeDim ? fg.d3Force('z', null) : null
      }
      physics.linkStrength && fg.d3Force('link').strength(physics.linkStrength)
      physics.linkIts && fg.d3Force('link').iterations(physics.linkIts)
      physics.charge && fg.d3Force('charge').strength(physics.charge)
      fg.d3Force('collide', physics.collision ? d3.forceCollide().radius(20) : null)
    })()
  })

  // Normally the graph doesn't update when you just change the physics parameters
  // This forces the graph to make a small update when you do
  useEffect(() => {
    graph2dRef.current?.d3ReheatSimulation()
  }, [physics])

  //shitty handler to check for doubleClicks
  const lastNodeClickRef = useRef(0)

  const onNodeClick = (node: NodeObject, event: any) => {
    const isDoubleClick = event.timeStamp - lastNodeClickRef.current < 400
    lastNodeClickRef.current = event.timeStamp

    if (isDoubleClick) {
      window.open('org-protocol://roam-node?node=' + node.id, '_self')
      return
    }

    setScope((currentScope) => ({
      ...currentScope,
      nodeIds: [...currentScope.nodeIds, node.id as string],
    }))
    return
  }
  // easing algorithms

  const [opacity, setOpacity] = useState<number>(1)
  const [fadeIn, cancel] = useAnimation((x) => setOpacity(x), {
    duration: physics.animationSpeed,
    algorithm: physics.algorithms[physics.algorithmName] ?? Easing.Cubic.InOut,
  })
  const [fadeOut, fadeOutCancel] = useAnimation(
    (x) => setOpacity(Math.min(opacity, -1 * (x - 1))),
    {
      duration: physics.animationSpeed,
      algorithm: physics.algorithms[physics.algorithmName] ?? Easing.Cubic.InOut,
    },
  )

  const lastHoverNode = useRef()
  useEffect(() => {
    console.log(physics.algorithms[physics.algorithmName])
    hoverNode && (lastHoverNode.current = hoverNode)
    if (!physics.highlightAnim) {
      return
    }
    if (hoverNode) {
      fadeIn()
    } else {
      // if (opacity > 0.7) {
      cancel()
      opacity > 0.5 ? fadeOut() : setOpacity(0)
      // } else {
      //    cancel()
      //    setOpacity(0)
      // }
    }
  }, [hoverNode])
  const theme = useTheme()
  //this was just easier than getting an actual package
  /* const convertHexToRGBA = (hexCode: string, opacity: number) => {
     *   let hex = hexCode.replace('#', '')

     *   if (hex.length === 3) {
     *     hex = `${hex[0]}${hex[0]}${hex[1]}${hex[1]}${hex[2]}${hex[2]}`
     *   }

     *   const r = parseInt(hex.substring(0, 2), 16)
     *   const g = parseInt(hex.substring(2, 4), 16)
     *   const b = parseInt(hex.substring(4, 6), 16)

     *   return `rgba(${r},${g},${b},${opacity})`
     * } */
  const graphCommonProps: ComponentPropsWithoutRef<typeof TForceGraph2D> = {
    graphData: scopedGraphData,
    width: windowWidth,
    height: windowHeight,
    backgroundColor: theme.white,
    nodeLabel: (node) => (node as OrgRoamNode).title,
    nodeColor: (node) => {
      if (!physics.colorful) {
        if (Object.keys(highlightedNodes).length === 0) {
          return 'rgb(100, 100, 100)'
        }
        return highlightedNodes[node.id!] ? theme.blue['500'] : 'rgb(50, 50, 50)'
      }

      const palette = ['pink', 'purple', 'blue', 'cyan', 'teal', 'green', 'yellow', 'orange', 'red']

      return theme.colors[
        palette[numbereWithinRange(linksByNodeId[node.id!]?.length ?? 0, 0, palette.length - 1)]
      ][500]
    },
    nodeRelSize: physics.nodeRel,
    nodeVal: (node) => {
      const links = linksByNodeId[node.id!] ?? []
      const basicSize = 3 + links.length
      if (physics.highlightAnim) {
        const wasNeighbor = (link) =>
          link.source === lastHoverNode.current?.id! || link.target === lastHoverNode.current?.id!
        const wasHighlightedNode = links.some(wasNeighbor)
        const highlightSize = highlightedNodes[node.id!]
          ? 1 + opacity * (physics.highlightNodeSize - 1)
          : lastHoverNode.current?.id! === node.id!
          ? 1 + opacity * (physics.highlightNodeSize - 1)
          : wasHighlightedNode
          ? 1 + opacity * (physics.highlightNodeSize - 1)
          : 1
        return basicSize * highlightSize
      } else {
        const highlightSize = highlightedNodes[node.id!] ? physics.highlightNodeSize : 1
        return basicSize * highlightSize
      }
    },
    nodeCanvasObject: (node, ctx, globalScale) => {
      if (!physics.labels) {
        return
      }
      if (globalScale <= physics.labelScale && !highlightedNodes[node.id!]) {
        return
      }

      const links = linksByNodeId[node.id!] ?? []
      const wasHighlightedNode = links.some(
        (link) =>
          link.source === lastHoverNode.current?.id! || link.target === lastHoverNode.current?.id!,
      )
      const nodeTitle = (node as OrgRoamNode).title!
      const label = nodeTitle.substring(0, Math.min(nodeTitle.length, 30))
      // const label = 'label'
      const fontSize = 12 / globalScale
      const textWidth = ctx.measureText(label).width
      const bckgDimensions = [textWidth * 1.1, fontSize].map((n) => n + fontSize * 0.5) as [
        number,
        number,
      ] // some padding

      const fadeFactor = Math.min((3 * (globalScale - physics.labelScale)) / physics.labelScale, 1)

      // draw label background
      const getLabelOpacity = () => {
        if (physics.highlightAnim) {
          return Object.keys(highlightedNodes).length === 0
            ? lastHoverNode.current?.id! === node.id
              ? 1
              : 1 * fadeFactor * (-1 * (0.5 * opacity - 1))
            : highlightedNodes[node.id!] || wasHighlightedNode
            ? 1
            : 1 * fadeFactor * (-1 * (0.5 * opacity - 1))
        } else {
          return Object.keys(highlightedNodes).length === 0
            ? 1 * fadeFactor
            : highlightedNodes[node.id!]
            ? 1
            : 1 * fadeFactor
        }
      }
      const backgroundOpacity = 0.5 * getLabelOpacity()
      ctx.fillStyle = `rgba(20, 20, 20, ${backgroundOpacity})`
      ctx.fillRect(
        node.x! - bckgDimensions[0] / 2,
        node.y! - bckgDimensions[1] / 2,
        ...bckgDimensions,
      )

      // draw label text
      const textOpacity = 2 * backgroundOpacity
      ctx.textAlign = 'center'
      ctx.textBaseline = 'middle'
      ctx.fillStyle = `rgb(255, 255, 255, ${textOpacity})`
      ctx.font = `${fontSize}px Sans-Serif`
      ctx.fillText(label, node.x!, node.y!)
    },
    nodeCanvasObjectMode: () => 'after',

    linkDirectionalParticles: physics.particles ? physics.particlesNumber : undefined,
    linkColor: (link) => {
      const linkIsHighlighted =
        (link.source as NodeObject).id! === centralHighlightedNode?.id! ||
        (link.target as NodeObject).id! === centralHighlightedNode?.id!
      if (physics.highlightAnim) {
        const linkWasHighlighted =
          (link.source as NodeObject).id! === lastHoverNode.current?.id! ||
          (link.target as NodeObject).id! === lastHoverNode.current?.id!
        return linkIsHighlighted
          ? theme.colors.purple['inter'](opacity) /*the.colors.purple[500]*/
          : linkWasHighlighted
          ? theme.colors.purple['inter'](opacity) /*the.colors.purple[500]*/
          : theme.colors.gray[500]
      } else {
        return linkIsHighlighted ? theme.colors.purple[500] : theme.colors.gray[500]
      }
    },
    linkWidth: (link) => {
      const linkIsHighlighted =
        (link.source as NodeObject).id! === centralHighlightedNode?.id! ||
        (link.target as NodeObject).id! === centralHighlightedNode?.id!
      if (!physics.highlightAnim) {
        return linkIsHighlighted ? physics.linkWidth * physics.highlightLinkSize : physics.linkWidth
      }
      const linkWasHighlighted =
        (link.source as NodeObject).id! === lastHoverNode.current?.id! ||
        (link.target as NodeObject).id! === lastHoverNode.current?.id!
      return linkIsHighlighted
        ? physics.linkWidth * (1 + opacity * (physics.highlightLinkSize - 1))
        : linkWasHighlighted
        ? physics.linkWidth * (1 + opacity * (physics.highlightLinkSize - 1))
        : physics.linkWidth
    },
    linkDirectionalParticleWidth: physics.particlesWidth,

    d3AlphaDecay: physics.alphaDecay,
    d3AlphaMin: physics.alphaMin,
    d3VelocityDecay: physics.velocityDecay,

    onNodeClick,
    onBackgroundClick: () => {
      setScope((currentScope) => ({
        ...currentScope,
        nodeIds: [],
      }))
    },
    onNodeHover: (node) => {
      if (!physics.hover) {
        return
      }
      setHoverNode(node)
    },
  }

  return (
    <div>
      {threeDim ? (
        <ForceGraph3D ref={graph3dRef} {...graphCommonProps} nodeThreeObjectExtend={true} />
      ) : (
        <ForceGraph2D ref={graph2dRef} {...graphCommonProps} />
      )}
    </div>
  )
}

function numbereWithinRange(num: number, min: number, max: number) {
  return Math.min(Math.max(num, min), max)
}
