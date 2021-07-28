import React, { ComponentPropsWithoutRef, useEffect, useRef, useState, useMemo } from 'react'
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

import { initialPhysics, initialFilter } from '../components/config'
import { Tweaks } from '../components/tweaks'

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

export default function Home(setEmacsTheme: any) {
  // only render on the client
  const [showPage, setShowPage] = useState(false)
  useEffect(() => {
    setShowPage(true)
  }, [])

  if (!showPage) {
    return null
  }

  return <GraphPage setEmacsTheme={setEmacsTheme} />
}

export function GraphPage(setEmacsTheme: any) {
  const [physics, setPhysics] = usePersistantState('physics', initialPhysics)
  const [filter, setFilter] = usePersistantState('filter', initialFilter)
  const [graphData, setGraphData] = useState<GraphData | null>(null)
  const [emacsNodeId, setEmacsNodeId] = useState<string | null>(null)

  const nodeByIdRef = useRef<NodeById>({})
  const linksByNodeIdRef = useRef<LinksByNodeId>({})

  const fetchGraphData = () => {
    return fetch('http://localhost:35901/graph')
      .then((res) => res.json())
      .then((orgRoamGraphData) => {
        console.log('fetching graphdata')
        parseGraphData(orgRoamGraphData)
      })
  }

  const parseGraphData = (orgRoamGraphData: OrgRoamGraphReponse) => {
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

  useEffect(() => {
    const socket = new WebSocket('ws://localhost:35903')
    socket.addEventListener('open', (e) => {
      console.log('Connection with Emacs established')
    })
    socket.addEventListener('message', (event) => {
      const message = JSON.parse(event.data)
      console.log(typeof message.type)
      switch (message.type) {
        case 'graphdata':
          console.log('hey')
          parseGraphData(message.data)
          break
        case 'theme':
          console.log('Received theme data')
          console.log(message.data)
          console.log(setEmacsTheme)
          setEmacsTheme.setEmacsTheme.setEmacsTheme(message.data)
          break
        case 'command':
          console.log('command')
          switch (message.data.commandName) {
            case 'follow':
              setEmacsNodeId(message.data.id)
              break
            case 'zoom': {
              const links = linksByNodeIdRef.current[message.data.id!] ?? []
              const nodes = Object.fromEntries(
                [
                  message.commandData.id! as string,
                  ...links.flatMap((link) => [link.source, link.target]),
                ].map((nodeId) => [nodeId, {}]),
              )
              /* zoomToFit(500, 200, (node: OrgRoamNode)=>nodes[node.id!]) */
              console.log(nodes)
            }
            default:
              console.log('oopsie whoopsie')
          }
      }
    })
    // fetchGraphData()
  }, [])

  useEffect(() => {
    if (!emacsNodeId) {
      return
    }
    //fetchGraphData()
  }, [emacsNodeId])

  const [threeDim, setThreeDim] = useState(false)

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
        }}
      />
      <Box position="absolute" alignItems="top">
        <Graph
          nodeById={nodeByIdRef.current!}
          linksByNodeId={linksByNodeIdRef.current!}
          {...{
            physics,
            graphData,
            threeDim,
            emacsNodeId,
            filter,
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
}

export const Graph = function (props: GraphProps) {
  const { physics, graphData, threeDim, linksByNodeId, filter, emacsNodeId, nodeById } = props

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
    switch (physics.follow) {
      case 'Local':
        setScope({ nodeIds: [emacsNodeId] })
        break
      case 'Zoom':
      default:
    }
  }, [emacsNodeId])

  const centralHighlightedNode = hoverNode
  const highlightedNodes = useMemo(() => {
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
  }, [centralHighlightedNode, linksByNodeId])

  const filteredNodes = useMemo(() => {
    return graphData.nodes.filter((node) => {
      const links = linksByNodeId[node.id as string] ?? []
      let showNode = true
      if (filter.orphans) {
        if (filter.parents) {
          showNode = links.length !== 0
        } else {
          if (links.length === 0) {
            showNode = false
          } else {
            if (
              links.length -
                links.filter((link) => link.type === 'parent' || link.type === 'cite').length ===
              0
            ) {
              showNode = false
            }
          }
        }
      }
      return showNode
    })
  }, [filter, graphData.nodes, linksByNodeId])

  const filteredLinks = useMemo(() => {
    return graphData.links.filter((linkArg) => {
      const link = linkArg as OrgRoamLink
      return link.type !== 'cite' && (filter.parents || link.type !== 'parent')
    })
  }, [filter, JSON.stringify(graphData.links)])

  const scopedNodes = useMemo(() => {
    return filteredNodes.filter((node) => {
      const links = linksByNodeId[node.id as string] ?? []
      return (
        scope.nodeIds.includes(node.id as string) ||
        links.some((link) => {
          return scope.nodeIds.includes(link.source) || scope.nodeIds.includes(link.target)
        })
      )
    })
  }, [filteredNodes, linksByNodeId, scope.nodeIds])

  const scopedNodeIds = scopedNodes.map((node) => node.id as string)

  const scopedLinks = useMemo(() => {
    return filteredLinks.filter((link) => {
      // we need to cover both because force-graph modifies the original data
      // but if we supply the original data on each render, the graph will re-render sporadically
      const sourceId = typeof link.source === 'object' ? link.source.id! : (link.source as string)
      const targetId = typeof link.target === 'object' ? link.target.id! : (link.target as string)

      return (
        scopedNodeIds.includes(sourceId as string) && scopedNodeIds.includes(targetId as string)
      )
    })
  }, [filteredLinks, scopedNodes])

  const scopedGraphData = useMemo(
    () =>
      scope.nodeIds.length === 0
        ? { nodes: filteredNodes, links: filteredLinks }
        : {
            nodes: scopedNodes,
            links: scopedLinks,
          },
    [filter, scope, JSON.stringify(Object.keys(nodeById))],
  )

  // make sure the camera position and zoom are fine when the list of nodes to render is changed
  useEffect(() => {
    // this setTimeout was added holistically because the behavior is better when we put
    // zoomToFit off a little bit
    setTimeout(() => {
      const fg = threeDim ? graph3dRef.current : graph2dRef.current
      fg?.zoomToFit(0, numbereWithinRange(20, 200, windowWidth / 8))
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

  const [opacity, setOpacity] = useState<number>(1)
  const [fadeIn, cancel] = useAnimation((x) => setOpacity(x), {
    duration: physics.animationSpeed,
    algorithm: physics.algorithms[physics.algorithmName],
  })
  const [fadeOut] = useAnimation((x) => setOpacity(Math.min(opacity, -1 * (x - 1))), {
    duration: physics.animationSpeed,
    algorithm: physics.algorithms[physics.algorithmName],
  })

  const lastHoverNode = useRef<NodeObject | null>(null)
  useEffect(() => {
    if (hoverNode) {
      lastHoverNode.current = hoverNode
    }
    if (!physics.highlightAnim) {
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
  const interPurple = useMemo(
    () => d3int.interpolate(theme.colors.gray[500], theme.colors.purple[500]),
    [theme],
  )
  const interGray = useMemo(
    () => d3int.interpolate(theme.colors.gray[500], theme.colors.gray[400]),
    [theme],
  )

  const highlightedLinks = useMemo(() => linksByNodeId[hoverNode?.id!] ?? [], [hoverNode])

  const previouslyHighlightedLinks = useMemo(
    () => linksByNodeId[lastHoverNode.current?.id!] ?? [],
    [hoverNode],
  )

  const previouslyHighlightedNodes = useMemo(
    () =>
      Object.fromEntries(
        [
          lastHoverNode.current?.id! as string,
          ...previouslyHighlightedLinks.flatMap((link) => [link.source, link.target]),
        ].map((nodeId) => [nodeId, {}]),
      ),
    [hoverNode, previouslyHighlightedLinks, lastHoverNode],
  )

  const graphCommonProps: ComponentPropsWithoutRef<typeof TForceGraph2D> = {
    graphData: scopedGraphData,
    width: windowWidth,
    height: windowHeight,
    backgroundColor: theme.white,
    nodeLabel: (node) => (node as OrgRoamNode).title,
    nodeColor: (node) => {
      if (!physics.colorful) {
        return previouslyHighlightedNodes[node.id!] || highlightedNodes[node.id!]
          ? interPurple(opacity)
          : interGray(opacity)
      }
      if (node.id === emacsNodeId) {
        return theme.colors['red'][500]
      }

      const palette = [
        'pink',
        'purple',
        'blue',
        'cyan',
        'teal',
        'green',
        'yellow',
        'orange',
        'red',
      ].filter((color) => !['red'].includes(color))
      // otherwise links with parents get shown as having one note
      const linklen = linksByNodeId[node.id!]?.length ?? 0
      const parentCiteNeighbors = linklen
        ? linksByNodeId[node.id!]?.filter((link) => link.type === 'parent' || link.type === 'cite')
            .length
        : 0
      const neighbors = filter.parents ? linklen : linklen - parentCiteNeighbors!

      return theme.colors[palette[numbereWithinRange(neighbors, 0, palette.length - 1)]][500]
    },
    nodeRelSize: physics.nodeRel,
    nodeVal: (node) => {
      const links = linksByNodeId[node.id!] ?? []
      const parentNeighbors = links.length
        ? links.filter((link) => link.type === 'parent' || link.type === 'cite').length
        : 0
      const basicSize = 3 + links.length - (!filter.parents ? parentNeighbors : 0)
      const highlightSize =
        highlightedNodes[node.id!] || previouslyHighlightedNodes[node.id!]
          ? 1 + opacity * (physics.highlightNodeSize - 1)
          : 1
      return basicSize * highlightSize
    },
    nodeCanvasObject: (node, ctx, globalScale) => {
      if (!node) {
        return
      }

      if (!physics.labels) {
        return
      }
      const links = linksByNodeId[node.id!] ?? []
      const wasHighlightedNode = previouslyHighlightedNodes[node.id!]

      if (
        (globalScale <= physics.labelScale || physics.labels === 1) &&
        !highlightedNodes[node.id!] &&
        !wasHighlightedNode
      ) {
        return
      }

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
        if (physics.labels === 1) {
          return opacity
        }
        if (globalScale <= physics.labelScale) {
          return opacity
        }

        return highlightedNodes[node.id!] || previouslyHighlightedNodes[node.id!]
          ? Math.max(fadeFactor, opacity)
          : 1 * fadeFactor * (-1 * (0.5 * opacity - 1))
      }
      if (physics.labels === 2 && (wasHighlightedNode || highlightedNodes[node.id!])) {
        const backgroundOpacity = 0.5 * getLabelOpacity()
        ctx.fillStyle = `rgba(20, 20, 20, ${backgroundOpacity})`
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
      ctx.fillStyle = `rgb(255, 255, 255, ${textOpacity})`
      ctx.font = `${fontSize}px Sans-Serif`
      ctx.fillText(label, node.x!, node.y!)
    },
    nodeCanvasObjectMode: () => 'after',

    linkDirectionalParticles: physics.particles ? physics.particlesNumber : undefined,
    linkColor: (link) => {
      const linkIsHighlighted = isLinkRelatedToNode(link, centralHighlightedNode)
      const linkWasHighlighted = isLinkRelatedToNode(link, lastHoverNode.current)
      return linkIsHighlighted || linkWasHighlighted ? interPurple(opacity) : theme.colors.gray[500]
    },
    linkWidth: (link) => {
      const linkIsHighlighted = isLinkRelatedToNode(link, centralHighlightedNode)
      const linkWasHighlighted = isLinkRelatedToNode(link, lastHoverNode.current)

      return linkIsHighlighted || linkWasHighlighted
        ? physics.linkWidth * (1 + opacity * (physics.highlightLinkSize - 1))
        : physics.linkWidth
    },
    linkDirectionalParticleWidth: physics.particlesWidth,

    d3AlphaDecay: physics.alphaDecay,
    d3AlphaMin: physics.alphaMin,
    d3VelocityDecay: physics.velocityDecay,

    onNodeClick: (node: NodeObject, event: any) => {
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
    },
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
        <ForceGraph3D
          ref={graph3dRef}
          {...graphCommonProps}
          nodeThreeObjectExtend={true}
          backgroundColor={theme.colors.white}
          nodeOpacity={physics.nodeOpacity}
          nodeResolution={physics.nodeResolution}
          linkOpacity={physics.linkOpacity}
        />
      ) : (
        <ForceGraph2D ref={graph2dRef} {...graphCommonProps} />
      )}
    </div>
  )
}

function isLinkRelatedToNode(link: LinkObject, centralHighlightedNode: NodeObject | null) {
  return (
    (link.source as NodeObject).id! === centralHighlightedNode?.id! ||
    (link.target as NodeObject).id! === centralHighlightedNode?.id!
  )
}

function numbereWithinRange(num: number, min: number, max: number) {
  return Math.min(Math.max(num, min), max)
}
