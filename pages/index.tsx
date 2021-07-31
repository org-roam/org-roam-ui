import React, {
  ComponentPropsWithoutRef,
  useEffect,
  useRef,
  useState,
  useMemo,
  useContext,
  forwardRef,
} from 'react'
import { usePersistantState } from '../util/persistant-state'
const d3promise = import('d3-force-3d')
import * as d3int from 'd3-interpolate'

import type {
  ForceGraph2D as TForceGraph2D,
  ForceGraph3D as TForceGraph3D,
} from 'react-force-graph'
import { OrgRoamGraphReponse, OrgRoamLink, OrgRoamNode } from '../api'
import { GraphData, NodeObject, LinkObject } from 'force-graph'

import { useWindowSize } from '@react-hook/window-size'
import { useAnimation } from '@lilib/hooks'

import { Box, useTheme } from '@chakra-ui/react'

import {
  initialPhysics,
  initialFilter,
  initialVisuals,
  initialBehavior,
  initialMouse,
  algos,
} from '../components/config'
import { Tweaks } from '../components/tweaks'

import { ThemeContext, ThemeContextProps } from './themecontext'
import SpriteText from 'three-spritetext'

import ReconnectingWebSocket from 'reconnecting-websocket'

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
export type NodesByFile = { [file: string]: OrgRoamNode[] | undefined }
export type Scope = {
  nodeIds: string[]
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
  const [filter, setFilter] = usePersistantState('filter', initialFilter)
  const [visuals, setVisuals] = usePersistantState('visuals', initialVisuals)
  const [graphData, setGraphData] = useState<GraphData | null>(null)
  const [emacsNodeId, setEmacsNodeId] = useState<string | null>(null)
  const [behavior, setBehavior] = usePersistantState('behavior', initialBehavior)
  const [mouse, setMouse] = usePersistantState('mouse', initialMouse)

  const nodeByIdRef = useRef<NodeById>({})
  const linksByNodeIdRef = useRef<LinksByNodeId>({})

  const updateGraphData = (orgRoamGraphData: OrgRoamGraphReponse) => {
    const nodesByFile = orgRoamGraphData.nodes.reduce<NodesByFile>((acc, node) => {
      return {
        ...acc,
        [node.file]: [...(acc[node.file] ?? []), node],
      }
    }, {})

    const fileLinks: OrgRoamLink[] = Object.keys(nodesByFile).flatMap((file) => {
      const nodesInFile = nodesByFile[file] ?? []
      // "file node" as opposed to "heading node"
      const fileNode = nodesInFile.find((node) => node.level === 0)
      const headingNodes = nodesInFile.filter((node) => node.level !== 0)

      if (!fileNode) {
        return []
      }

      return headingNodes.map((headingNode) => ({
        source: headingNode.id,
        target: fileNode.id,
        type: 'parent',
      }))
    })

    nodeByIdRef.current = Object.fromEntries(orgRoamGraphData.nodes.map((node) => [node.id, node]))

    const links = [...orgRoamGraphData.links, ...fileLinks]
    linksByNodeIdRef.current = links.reduce<LinksByNodeId>((acc, link) => {
      return {
        ...acc,
        [link.source]: [...(acc[link.source] ?? []), link],
        [link.target]: [...(acc[link.target] ?? []), link],
      }
    }, {})

    const orgRoamGraphDataWithFileLinks = {
      ...orgRoamGraphData,
      links,
    }

    // react-force-graph modifies the graph data implicitly,
    // so we make sure there's no overlap between the objects we pass it and
    // nodeByIdRef, linksByNodeIdRef
    const orgRoamGraphDataClone = JSON.parse(JSON.stringify(orgRoamGraphDataWithFileLinks))
    setGraphData(orgRoamGraphDataClone)
  }

  const { setEmacsTheme } = useContext(ThemeContext)

  const [threeDim, setThreeDim] = useState(false)
  const [scope, setScope] = useState<Scope>({ nodeIds: [] })

  const graphRef = useRef<any>(null)

  const followBehavior = (
    command: string,
    emacsNode: string,
    speed: number = 2000,
    padding: number = 200,
  ) => {
    const fg = graphRef.current
    const links = linksByNodeIdRef.current[emacsNode] ?? []
    const nodes = Object.fromEntries(
      [emacsNode as string, ...links.flatMap((link) => [link.source, link.target])].map(
        (nodeId) => [nodeId, {}],
      ),
    )
    if (command === 'zoom') {
      if (scope.nodeIds.length) {
        setScope({ nodeIds: [] })
      }
      setTimeout(() => fg.zoomToFit(speed, padding, (node: OrgRoamNode) => nodes[node.id!]), 50)
      return
    }
    if (!scope.nodeIds.length) {
      setScope({ nodeIds: [emacsNode] })
      setTimeout(() => {
        fg.zoomToFit(speed, padding, (node: OrgRoamNode) => nodes[node.id!])
      }, 50)
      return
    }
    if (behavior.localSame !== 'add') {
      setScope({ nodeIds: [emacsNode] })
      setTimeout(() => {
        fg.zoomToFit(speed, padding, (node: OrgRoamNode) => nodes[node.id!])
      }, 50)
      return
    }

    // if the node is in the scopednodes, add it to scope instead of replacing it
    if (
      !scope.nodeIds.includes(emacsNode) ||
      !scope.nodeIds.some((scopeId: string) => {
        return nodes[scopeId]
      })
    ) {
      setScope({ nodeIds: [emacsNode] })
      setTimeout(() => {
        fg.zoomToFit(speed, padding, (node: OrgRoamNode) => nodes[node.id!])
      }, 50)
      return
    }
    setScope((currentScope: Scope) => ({
      ...currentScope,
      nodeIds: [...currentScope.nodeIds, emacsNode as string],
    }))
    setTimeout(() => fg.zoomToFit(speed, padding, (node: OrgRoamNode) => nodes[node.id!]), 50)
  }

  useEffect(() => {
    const socket = new ReconnectingWebSocket('ws://localhost:35903')
    socket.addEventListener('open', (event) => {
      console.log('Connection with Emacs established')
    })
    socket.addEventListener('message', (event) => {
      const fg = graphRef.current
      const message = JSON.parse(event.data)
      switch (message.type) {
        case 'graphdata':
          return updateGraphData(message.data)
        case 'theme':
          return setEmacsTheme(message.data)
        case 'command':
          switch (message.data.commandName) {
            case 'local':
              const speed = behavior.zoomSpeed
              const padding = behavior.zoomPadding
              followBehavior('local', message.data.id, speed, padding)
              setEmacsNodeId(message.data.id)
              break
            case 'zoom': {
              const speed = message?.data?.speed || behavior.zoomSpeed
              const padding = message?.data?.padding || behavior.zoomPadding
              followBehavior('zoom', message.data.id, speed, padding)
              setEmacsNodeId(message.data.id)
              break
            }
            case 'follow': {
              followBehavior(
                behavior.follow,
                message.data.id,
                behavior.zoomSpeed,
                behavior.zoomPadding,
              )
              setEmacsNodeId(message.data.id)
              break
            }
            default:
              return console.error('unknown message type', message.type)
          }
      }
    })
  }, [])

  if (!graphData) {
    return null
  }

  return (
    <Box display="flex" alignItems="flex-start" flexDirection="row" height="100%">
      <Tweaks
        {...{
          physics,
          setPhysics,
          threeDim,
          setThreeDim,
          filter,
          setFilter,
          visuals,
          setVisuals,
          mouse,
          setMouse,
          behavior,
          setBehavior,
        }}
      />
      <Box position="absolute" alignItems="top">
        <Graph
          ref={graphRef}
          nodeById={nodeByIdRef.current!}
          linksByNodeId={linksByNodeIdRef.current!}
          {...{
            physics,
            graphData,
            threeDim,
            emacsNodeId,
            filter,
            visuals,
            behavior,
            mouse,
            scope,
            setScope,
          }}
        />
      </Box>
    </Box>
  )
}

export interface GraphProps {
  nodeById: NodeById
  linksByNodeId: LinksByNodeId
  graphData: GraphData
  physics: typeof initialPhysics
  threeDim: boolean
  filter: typeof initialFilter
  emacsNodeId: string | null
  visuals: typeof initialVisuals
  behavior: typeof initialBehavior
  mouse: typeof initialMouse
  scope: Scope
  setScope: any
}

export const Graph = forwardRef(function (props: GraphProps, graphRef: any) {
  const {
    physics,
    graphData,
    threeDim,
    linksByNodeId,
    filter,
    emacsNodeId,
    nodeById,
    visuals,
    behavior,
    mouse,
    scope,
    setScope,
  } = props

  // react-force-graph does not track window size
  // https://github.com/vasturiano/react-force-graph/issues/233
  // does not work below a certain width
  const [windowWidth, windowHeight] = useWindowSize()

  const [hoverNode, setHoverNode] = useState<NodeObject | null>(null)

  const handleClick = (click: string, node: NodeObject) => {
    switch (click) {
      //mouse.highlight:
      case mouse.local: {
        if (scope.nodeIds.includes(node.id as string)) {
          break
        }
        setScope((currentScope: Scope) => ({
          ...currentScope,
          nodeIds: [...currentScope.nodeIds, node.id as string],
        }))
        break
      }
      case mouse.follow: {
        window.open('org-protocol://roam-node?node=' + node.id, '_self')
        break
      }
      default:
        break
    }
  }
  const getNeighborNodes = (id: string) => {
    const links = linksByNodeId[id]! ?? []
    return Object.fromEntries(
      [id as string, ...links.flatMap((link) => [link.source, link.target])].map((nodeId) => [
        nodeId,
        {},
      ]),
    )
  }

  const centralHighlightedNode = useRef<NodeObject | null>(null)

  useEffect(() => {
    if (!emacsNodeId) {
      return
    }
    centralHighlightedNode.current = nodeById[emacsNodeId] as NodeObject
  }, [emacsNodeId])

  centralHighlightedNode.current = hoverNode
  const highlightedNodes = useMemo(() => {
    if (!centralHighlightedNode.current) {
      return {}
    }

    const links = linksByNodeId[centralHighlightedNode.current.id!]
    if (!links) {
      return {}
    }

    return Object.fromEntries(
      [
        centralHighlightedNode.current.id! as string,
        ...links.flatMap((link) => [link.source, link.target]),
      ].map((nodeId) => [nodeId, {}]),
    )
  }, [centralHighlightedNode.current, linksByNodeId])

  const scopedGraphData = useMemo(() => {
    const filteredNodes = graphData.nodes.filter((node) => {
      const links = linksByNodeId[node.id as string] ?? []
      if (!filter.orphans) {
        return true
      }

      if (filter.parents) {
        return links.length !== 0
      }

      if (links.length === 0) {
        return false
      }

      return links.some((link) => !['parent', 'cite', 'ref'].includes(link.type))
    })

    const filteredLinks = graphData.links.filter((linkArg) => {
      const link = linkArg as OrgRoamLink
      return link.type !== 'cite' && (filter.parents || link.type !== 'parent')
    })

    const scopedNodes = filteredNodes.filter((node) => {
      const links = linksByNodeId[node.id as string] ?? []
      return (
        scope.nodeIds.includes(node.id as string) ||
        links.some((link) => {
          return scope.nodeIds.includes(link.source) || scope.nodeIds.includes(link.target)
        })
      )
    })

    const scopedNodeIds = scopedNodes.map((node) => node.id as string)

    const scopedLinks = filteredLinks.filter((link) => {
      // we need to cover both because force-graph modifies the original data
      // but if we supply the original data on each render, the graph will re-render sporadically
      const sourceId = typeof link.source === 'object' ? link.source.id! : (link.source as string)
      const targetId = typeof link.target === 'object' ? link.target.id! : (link.target as string)

      return (
        scopedNodeIds.includes(sourceId as string) && scopedNodeIds.includes(targetId as string)
      )
    })

    return scope.nodeIds.length === 0
      ? { nodes: filteredNodes, links: filteredLinks }
      : {
          nodes: scopedNodes,
          links: scopedLinks,
        }
  }, [filter, scope, graphData])

  useEffect(() => {
    ;(async () => {
      const fg = graphRef.current
      const d3 = await d3promise
      if (physics.gravityOn) {
        fg.d3Force('x', d3.forceX().strength(physics.gravity))
        fg.d3Force('y', d3.forceY().strength(physics.gravity))
        threeDim && fg.d3Force('z', d3.forceZ().strength(physics.gravity))
      } else {
        fg.d3Force('x', null)
        fg.d3Force('y', null)
        threeDim && fg.d3Force('z', null)
      }
      physics.centering
        ? fg.d3Force('center', d3.forceCenter().strength(physics.centeringStrength))
        : fg.d3Force('center', null)
      physics.linkStrength && fg.d3Force('link').strength(physics.linkStrength)
      physics.linkIts && fg.d3Force('link').iterations(physics.linkIts)
      physics.charge && fg.d3Force('charge').strength(physics.charge)
      fg.d3Force(
        'collide',
        physics.collision ? d3.forceCollide().radius(physics.collisionStrength) : null,
      )
    })()
  })

  // Normally the graph doesn't update when you just change the physics parameters
  // This forces the graph to make a small update when you do
  useEffect(() => {
    graphRef.current?.d3ReheatSimulation()
  }, [physics])

  //shitty handler to check for doubleClicks
  const lastNodeClickRef = useRef(0)

  const [opacity, setOpacity] = useState<number>(1)
  const [fadeIn, cancel] = useAnimation((x) => setOpacity(x), {
    duration: visuals.animationSpeed,
    algorithm: algos[visuals.algorithmName],
  })
  const [fadeOut, fadeOutCancel] = useAnimation(
    (x) => setOpacity(Math.min(opacity, -1 * (x - 1))),
    {
      duration: visuals.animationSpeed,
      algorithm: algos[visuals.algorithmName],
    },
  )

  const lastHoverNode = useRef<OrgRoamNode | null>(null)
  useEffect(() => {
    if (hoverNode) {
      lastHoverNode.current = hoverNode as OrgRoamNode
    }
    if (!visuals.highlightAnim) {
      return hoverNode ? setOpacity(1) : setOpacity(0)
    }
    if (hoverNode) {
      fadeIn()
    } else {
      // to prevent fadeout animation from starting at 1
      // when quickly moving away from a hovered node
      cancel()
      opacity > 0.5 ? fadeOut() : setOpacity(0)
    }
  }, [hoverNode])

  const theme = useTheme()
  const themeContext = useContext<ThemeContextProps>(ThemeContext)

  const getThemeColor = (name: string) => {
    if (!theme) {
      return
    }
    return name.split('.').reduce((o, i) => o[i], theme.colors)
  }

  const highlightColors = useMemo(() => {
    const allColors = visuals.nodeColorScheme.concat(
      visuals.linkColorScheme || [],
      visuals.linkHighlight || [],
      visuals.nodeHighlight || [],
    )

    return Object.fromEntries(
      allColors.map((color) => {
        const color1 = getThemeColor(color)
        const crisscross = allColors.map((color2) => [
          color2,
          d3int.interpolate(color1, getThemeColor(color2)),
        ])
        return [color, Object.fromEntries(crisscross)]
      }),
    )
  }, [
    visuals.nodeColorScheme,
    visuals.linkHighlight,
    visuals.nodeHighlight,
    visuals.linkColorScheme,
  ])

  const previouslyHighlightedNodes = useMemo(() => {
    const previouslyHighlightedLinks = linksByNodeId[lastHoverNode.current?.id!] ?? []
    return Object.fromEntries(
      [
        lastHoverNode.current?.id! as string,
        ...previouslyHighlightedLinks.flatMap((link) => [link.source, link.target]),
      ].map((nodeId) => [nodeId, {}]),
    )
  }, [JSON.stringify(hoverNode), lastHoverNode.current])

  const getNodeColorById = (id: string) => {
    const linklen = linksByNodeId[id!]?.length ?? 0
    const parentCiteNeighbors = linklen
      ? linksByNodeId[id!]?.filter((link) => link.type === 'parent' || link.type === 'cite').length
      : 0
    const neighbors = filter.parents ? linklen : linklen - parentCiteNeighbors!

    return visuals.nodeColorScheme[
      numberWithinRange(neighbors, 0, visuals.nodeColorScheme.length - 1)
    ]
  }
  const getLinkNodeColor = (sourceId: string, targetId: string) => {
    return linksByNodeId[sourceId]! > linksByNodeId[targetId]!
      ? getNodeColorById(sourceId)
      : getNodeColorById(targetId)
  }

  const getLinkColor = (sourceId: string, targetId: string, needsHighlighting: boolean) => {
    // I'm so sorry
    // if we are matching the node color and don't have a highlight color
    // or we don't have our own scheme and we're not being highlighted
    if (!visuals.linkHighlight && !visuals.linkColorScheme && !needsHighlighting) {
      const nodeColor = getLinkNodeColor(sourceId, targetId)
      return getThemeColor(nodeColor)
    }

    if (!needsHighlighting && !visuals.linkColorScheme) {
      const nodeColor = getLinkNodeColor(sourceId, targetId)
      return getThemeColor(nodeColor)
    }

    if (!needsHighlighting) {
      return getThemeColor(visuals.linkColorScheme)
    }

    if (!visuals.linkHighlight && !visuals.linkColorScheme) {
      const nodeColor = getLinkNodeColor(sourceId, targetId)
      return getThemeColor(nodeColor)
    }

    if (!visuals.linkHighlight) {
      return getThemeColor(visuals.linkColorScheme)
    }

    if (!visuals.linkColorScheme) {
      return highlightColors[getLinkNodeColor(sourceId, targetId)][visuals.linkHighlight](opacity)
    }

    return highlightColors[visuals.linkColorScheme][visuals.linkHighlight](opacity)
  }

  const getNodeColor = (node: OrgRoamNode) => {
    const needsHighlighting = highlightedNodes[node.id!] || previouslyHighlightedNodes[node.id!]
    // if we are matching the node color and don't have a highlight color
    // or we don't have our own scheme and we're not being highlighted
    if (visuals.emacsNodeColor && node.id === emacsNodeId) {
      return getThemeColor(visuals.emacsNodeColor)
    }
    if (!needsHighlighting) {
      return getThemeColor(getNodeColorById(node.id))
    }
    if (!visuals.nodeHighlight) {
      return getThemeColor(getNodeColorById(node.id))
    }
    return highlightColors[getNodeColorById(node.id)][visuals.nodeHighlight](opacity)
  }

  const hexToRGBA = (hex: string, opacity: number) =>
    'rgba(' +
    (hex = hex.replace('#', ''))
      .match(new RegExp('(.{' + hex.length / 3 + '})', 'g'))!
      .map(function (l) {
        return parseInt(hex.length % 2 ? l + l : l, 16)
      })
      .concat(isFinite(opacity) ? opacity : 1)
      .join(',') +
    ')'

  const labelTextColor = useMemo(
    () => getThemeColor(visuals.labelTextColor),
    [visuals.labelTextColor],
  )
  const labelBackgroundColor = useMemo(
    () => getThemeColor(visuals.labelBackgroundColor),
    [visuals.labelBackgroundColor],
  )

  const graphCommonProps: ComponentPropsWithoutRef<typeof TForceGraph2D> = {
    graphData: scopedGraphData,
    width: windowWidth,
    height: windowHeight,
    backgroundColor: theme.colors.gray[visuals.backgroundColor],
    nodeLabel: (node) => (node as OrgRoamNode).title,
    nodeColor: (node) => {
      return getNodeColor(node as OrgRoamNode)
    },
    nodeRelSize: visuals.nodeRel,
    nodeVal: (node) => {
      const links = linksByNodeId[node.id!] ?? []
      const parentNeighbors = links.length
        ? links.filter((link) => link.type === 'parent' || link.type === 'cite').length
        : 0
      const basicSize = 3 + links.length - (!filter.parents ? parentNeighbors : 0)
      const highlightSize =
        highlightedNodes[node.id!] || previouslyHighlightedNodes[node.id!]
          ? 1 + opacity * (visuals.highlightNodeSize - 1)
          : 1
      return basicSize * highlightSize
    },
    nodeCanvasObject: (node, ctx, globalScale) => {
      if (!node) {
        return
      }

      if (!visuals.labels) {
        return
      }
      const wasHighlightedNode = previouslyHighlightedNodes[node.id!]

      if (
        (globalScale <= visuals.labelScale || visuals.labels === 1) &&
        !highlightedNodes[node.id!] &&
        !wasHighlightedNode
      ) {
        return
      }

      const nodeTitle = (node as OrgRoamNode).title!
      const label = nodeTitle.substring(0, Math.min(nodeTitle.length, 40))
      // const label = 'label'
      const fontSize = 12 / globalScale
      const textWidth = ctx.measureText(label).width
      const bckgDimensions = [textWidth * 1.1, fontSize].map((n) => n + fontSize * 0.5) as [
        number,
        number,
      ] // some padding

      const fadeFactor = Math.min((3 * (globalScale - visuals.labelScale)) / visuals.labelScale, 1)

      // draw label background
      const getLabelOpacity = () => {
        if (visuals.labels === 1) {
          return opacity
        }
        if (globalScale <= visuals.labelScale) {
          return opacity
        }
        return highlightedNodes[node.id!] || previouslyHighlightedNodes[node.id!]
          ? Math.max(fadeFactor, opacity)
          : 1 * fadeFactor * (-1 * (0.5 * opacity - 1))
      }

      if (visuals.labelBackgroundColor && visuals.labelBackgroundOpacity) {
        const backgroundOpacity = getLabelOpacity() * visuals.labelBackgroundOpacity
        const labelBackground = hexToRGBA(labelBackgroundColor, backgroundOpacity)
        ctx.fillStyle = labelBackground
        ctx.fillRect(
          node.x! - bckgDimensions[0] / 2,
          node.y! - bckgDimensions[1] / 2,
          ...bckgDimensions,
        )
      }

      // draw label text
      const textOpacity = getLabelOpacity()
      ctx.textAlign = 'center'
      ctx.textBaseline = 'middle'
      const labelText = hexToRGBA(labelTextColor, textOpacity)
      ctx.fillStyle = labelText
      ctx.font = `${fontSize}px Sans-Serif`
      ctx.fillText(label, node.x!, node.y!)
    },
    nodeCanvasObjectMode: () => 'after',

    linkDirectionalParticles: visuals.particles ? visuals.particlesNumber : undefined,
    linkColor: (link) => {
      const sourceId = typeof link.source === 'object' ? link.source.id! : (link.source as string)
      const targetId = typeof link.target === 'object' ? link.target.id! : (link.target as string)
      const linkIsHighlighted = isLinkRelatedToNode(link, centralHighlightedNode.current)
      const linkWasHighlighted = isLinkRelatedToNode(link, lastHoverNode.current)
      const needsHighlighting = linkIsHighlighted || linkWasHighlighted
      return getLinkColor(sourceId as string, targetId as string, needsHighlighting)
    },
    linkWidth: (link) => {
      const linkIsHighlighted = isLinkRelatedToNode(link, centralHighlightedNode.current)
      const linkWasHighlighted = isLinkRelatedToNode(link, lastHoverNode.current)

      return linkIsHighlighted || linkWasHighlighted
        ? visuals.linkWidth * (1 + opacity * (visuals.highlightLinkSize - 1))
        : visuals.linkWidth
    },
    linkDirectionalParticleWidth: visuals.particlesWidth,

    d3AlphaDecay: physics.alphaDecay,
    d3AlphaMin: physics.alphaMin,
    d3VelocityDecay: physics.velocityDecay,

    onNodeClick: (node: NodeObject, event: any) => {
      const isDoubleClick = event.timeStamp - lastNodeClickRef.current < 400
      lastNodeClickRef.current = event.timeStamp
      if (isDoubleClick) {
        return handleClick('double', node)
      }
      return handleClick('click', node)
    },
    onBackgroundClick: () => {
      setHoverNode(null)
      if (scope.nodeIds.length === 0) {
        return
      }
      setScope((currentScope) => ({
        ...currentScope,
        nodeIds: [],
      }))
    },
    onNodeHover: (node) => {
      if (!visuals.highlight) {
        return
      }

      if (!hoverNode) {
        fadeOutCancel()
        setOpacity(0)
      }
      setHoverNode(node)
    },
    onNodeRightClick: (node) => {
      handleClick('right', node)
    },
  }

  return (
    <div>
      {threeDim ? (
        <ForceGraph3D
          ref={graphRef}
          {...graphCommonProps}
          nodeThreeObjectExtend={true}
          backgroundColor={theme.colors.white}
          nodeOpacity={visuals.nodeOpacity}
          nodeResolution={visuals.nodeResolution}
          linkOpacity={visuals.linkOpacity}
          nodeThreeObject={(node: OrgRoamNode) => {
            if (!visuals.labels) {
              return
            }
            if (visuals.labels < 3 && !highlightedNodes[node.id!]) {
              return
            }
            const sprite = new SpriteText(node.title.substring(0, 40))
            sprite.color = getThemeColor(visuals.labelTextColor)
            sprite.backgroundColor = getThemeColor(visuals.labelBackgroundColor)
            sprite.padding = 2
            sprite.textHeight = 8

            return sprite
          }}
        />
      ) : (
        <ForceGraph2D ref={graphRef} {...graphCommonProps} />
      )}
    </div>
  )
})

function isLinkRelatedToNode(link: LinkObject, node: NodeObject | null) {
  return (
    (link.source as NodeObject).id! === node?.id! || (link.target as NodeObject).id! === node?.id!
  )
}

function numberWithinRange(num: number, min: number, max: number) {
  return Math.min(Math.max(num, min), max)
}
