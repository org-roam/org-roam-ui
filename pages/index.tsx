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

import { Box, useDisclosure, useTheme } from '@chakra-ui/react'

import {
  initialPhysics,
  initialFilter,
  initialVisuals,
  initialBehavior,
  initialMouse,
  algos,
  TagColors,
  colorList,
} from '../components/config'
import { Tweaks } from '../components/tweaks'
import { ContextMenu } from '../components/contextmenu'

import { ThemeContext, ThemeContextProps } from '../util/themecontext'
import SpriteText from 'three-spritetext'

import ReconnectingWebSocket from 'reconnecting-websocket'
import { sendJson } from 'next/dist/next-server/server/api-utils'

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
export type Tags = string[]
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

function normalizeLinkEnds(link: OrgRoamLink | LinkObject): [string, string] {
  // we need to cover both because force-graph modifies the original data
  // but if we supply the original data on each render, the graph will re-render sporadically
  const sourceId =
    typeof link.source === 'object' ? (link.source.id! as string) : (link.source as string)
  const targetId =
    typeof link.target === 'object' ? (link.target.id! as string) : (link.target as string)
  return [sourceId, targetId]
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
  const tagsRef = useRef<Tags>([])
  const graphRef = useRef<any>(null)

  const currentGraphDataRef = useRef<GraphData>({ nodes: [], links: [] })

  const updateGraphData = (orgRoamGraphData: OrgRoamGraphReponse) => {
    const currentGraphData = currentGraphDataRef.current
    const oldNodeById = nodeByIdRef.current
    const oldLinksByNodeId = linksByNodeIdRef.current
    tagsRef.current = orgRoamGraphData.tags ?? []
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

    const dirtyLinks = [...orgRoamGraphData.links, ...fileLinks]
    const nonExistantNodes: OrgRoamNode[] = []
    const links = dirtyLinks.map((link) => {
      const sourceId = link.source as string
      const targetId = link.target as string
      if (!nodeByIdRef.current[sourceId]) {
        nonExistantNodes.push({
          id: sourceId,
          tags: ['bad'],
          properties: { FILELESS: 'yes', bad: 'yes' },
          file: '',
          title: sourceId,
          level: 0,
        })
        return { ...link, type: 'bad' }
      }
      if (!nodeByIdRef.current[targetId]) {
        nonExistantNodes.push({
          id: targetId,
          tags: ['bad'],
          properties: { FILELESS: 'yes', bad: 'yes' },
          file: '',
          title: targetId,
          level: 0,
        })
        return { ...link, type: 'bad' }
      }
      return link
    })

    nodeByIdRef.current = {
      ...nodeByIdRef.current,
      ...Object.fromEntries(nonExistantNodes.map((node) => [node.id, node])),
    }

    linksByNodeIdRef.current = links.reduce<LinksByNodeId>((acc, link) => {
      return {
        ...acc,
        [link.source]: [...(acc[link.source] ?? []), link],
        [link.target]: [...(acc[link.target] ?? []), link],
      }
    }, {})

    const nodes = [...orgRoamGraphData.nodes, ...nonExistantNodes]
    const orgRoamGraphDataWithFileLinksAndBadNdes = {
      nodes,
      links,
    }

    if (!currentGraphData.nodes.length) {
      // react-force-graph modifies the graph data implicitly,
      // so we make sure there's no overlap between the objects we pass it and
      // nodeByIdRef, linksByNodeIdRef
      const orgRoamGraphDataClone = JSON.parse(
        JSON.stringify(orgRoamGraphDataWithFileLinksAndBadNdes),
      )
      console.log(orgRoamGraphDataClone)
      currentGraphDataRef.current = orgRoamGraphDataClone
      setGraphData(orgRoamGraphDataClone)
      return
    }

    const newNodes = [
      ...currentGraphData.nodes.map((node: NodeObject) => {
        const newNode = nodeByIdRef.current[node?.id!] ?? false
        if (!newNode) {
          return
        }
        return { ...node, ...newNode }
      }),
      ...Object.keys(nodeByIdRef.current)
        .filter((id) => !oldNodeById[id])
        .map((id) => {
          console.log(id)
          return nodeByIdRef.current[id] as NodeObject
        }),
    ]

    const nodeIndex = newNodes.reduce<{ [id: string]: number }>((acc, node, index) => {
      const id = node?.id as string
      return {
        ...acc,
        [id]: index,
      }
    }, {})
    console.log(newNodes)
    console.log(nodeIndex)
    /* const currentGraphIndexByLink = currentGraphData.links.reduce<{[key: string]: number}>((acc, link, index) => {
*   const [source, target] = normalizeLinkEnds(link)
*     const sourceTarget=source+target
*     return  {
*         ...acc,
*  [sourceTarget]: index
* }
},{}) */
    /* const newLinks = [
     *   ...currentGraphData!.links.filter((link) => {
     *     const [source, target] = normalizeLinkEnds(link)
     *     if (!nodeByIdRef.current[source] || !nodeByIdRef.current[target]) {
     *       return false
     *     }
     *     if (
     *       !linksByNodeIdRef.current[source]!.some(
     *         (link) => link.target === target || link.source === target,
     *       ) &&
     *       !linksByNodeIdRef.current[target]!.some(
     *         (link) => link.target === source || link.source === source,
     *       )
     *     ) {
     *       return false
     *     }
     *     return true
     *   }),
     *   ...Object.keys(linksByNodeIdRef.current).flatMap((id) => {
     *     if (!oldLinksByNodeId[id]!) {
     *       return linksByNodeIdRef.current![id!]!
     *     }
     *     return (
     *       linksByNodeIdRef.current![id]!.filter((link) => {
     *         const [source, target] = normalizeLinkEnds(link)
     *         return !oldLinksByNodeId[id]!.some(
     *           (oldLink) => oldLink.source === source && oldLink.target === target,
     *         )!
     *       }) ?? []
     *     )
     *   }),
     * ]
     */
    const newerLinks = links.map((link) => {
      const [source, target] = normalizeLinkEnds(link)
      return {
        ...link,
        source: newNodes[nodeIndex![source]],
        target: newNodes[nodeIndex![target]],
      }
    })
    const fg = graphRef.current
    setGraphData({ nodes: newNodes as NodeObject[], links: newerLinks })
  }
  useEffect(() => {
    if (!graphData) {
      return
    }
    currentGraphDataRef.current = graphData
  }, [graphData])

  const { setEmacsTheme } = useContext(ThemeContext)

  const [threeDim, setThreeDim] = usePersistantState('3d', false)
  const [tagColors, setTagColors] = usePersistantState<TagColors>('tagCols', {})
  const [scope, setScope] = useState<Scope>({ nodeIds: [] })
  const scopeRef = useRef<Scope>({ nodeIds: [] })
  const behaviorRef = useRef(initialBehavior)
  behaviorRef.current = behavior
  const WebSocketRef = useRef<any>(null)

  scopeRef.current = scope
  const followBehavior = (
    command: string,
    emacsNode: string,
    speed: number = 2000,
    padding: number = 200,
  ) => {
    if (command === 'color') {
      return
    }
    const fg = graphRef.current
    const sr = scopeRef.current
    const bh = behaviorRef.current
    const links = linksByNodeIdRef.current[emacsNode] ?? []
    const nodes = Object.fromEntries(
      [emacsNode as string, ...links.flatMap((link) => [link.source, link.target])].map(
        (nodeId) => [nodeId, {}],
      ),
    )
    if (command === 'zoom') {
      console.log(sr)
      if (sr.nodeIds.length) {
        console.log('emptying')
        console.log('scope ' + sr.nodeIds)
        setScope({ nodeIds: [] })
      }
      setTimeout(() => fg.zoomToFit(speed, padding, (node: OrgRoamNode) => nodes[node.id!]), 50)
      return
    }
    if (!sr.nodeIds.length) {
      setScope({ nodeIds: [emacsNode] })
      setTimeout(() => {
        /* fg.zoomToFit(speed, padding, (node: OrgRoamNode) => nodes[node.id!]) */
        fg.centerAt(0, 0, speed)
      }, 50)
      return
    }
    if (bh.localSame !== 'add') {
      setScope({ nodeIds: [emacsNode] })
      setTimeout(() => {
        /* fg.zoomToFit(speed, padding, (node: OrgRoamNode) => nodes[node.id!]) */
        fg.centerAt(0, 0, speed)
      }, 50)
      return
    }

    // if the node is in the scopednodes, add it to scope instead of replacing it
    if (
      !sr.nodeIds.includes(emacsNode) ||
      !sr.nodeIds.some((scopeId: string) => {
        return nodes[scopeId]
      })
    ) {
      setScope({ nodeIds: [emacsNode] })
      setTimeout(() => {
        /* fg.zoomToFit(speed, padding, (node: OrgRoamNode) => nodes[node.id!]) */
        fg.centerAt(0, 0, speed)
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
    WebSocketRef.current = new ReconnectingWebSocket('ws://localhost:35903')
    WebSocketRef.current.addEventListener('open', (event: any) => {
      console.log('Connection with Emacs established')
    })
    WebSocketRef.current.addEventListener('message', (event: any) => {
      const fg = graphRef.current
      const bh = behaviorRef.current
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
              const speed = message?.data?.speed || bh.zoomSpeed
              const padding = message?.data?.padding || bh.zoomPadding
              followBehavior('zoom', message.data.id, speed, padding)
              setEmacsNodeId(message.data.id)
              break
            }
            case 'follow': {
              followBehavior(bh.follow, message.data.id, bh.zoomSpeed, bh.zoomPadding)
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
    <Box display="flex" alignItems="flex-start" flexDirection="row" height="100%" overflow="hidden">
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
          tagColors,
          setTagColors,
        }}
        tags={tagsRef.current}
      />
      <Box position="absolute" alignItems="top" overflow="hidden">
        <Graph
          ref={graphRef}
          nodeById={nodeByIdRef.current!}
          linksByNodeId={linksByNodeIdRef.current!}
          webSocket={WebSocketRef.current}
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
            tagColors,
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
  webSocket: any
  tagColors: { [tag: string]: string }
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
    webSocket,
    tagColors,
  } = props

  // react-force-graph does not track window size
  // https://github.com/vasturiano/react-force-graph/issues/233
  // does not work below a certain width
  const [windowWidth, windowHeight] = useWindowSize()

  const [hoverNode, setHoverNode] = useState<NodeObject | null>(null)

  const theme = useTheme()

  const { emacsTheme } = useContext<ThemeContextProps>(ThemeContext)

  const handleLocal = (node: OrgRoamNode, add: string) => {
    if (scope.nodeIds.includes(node.id as string)) {
      return
    }
    if (add === 'replace') {
      setScope({ nodeIds: [node.id] })
      return
    }
    setScope((currentScope: Scope) => ({
      ...currentScope,
      nodeIds: [...currentScope.nodeIds, node.id as string],
    }))
    return
  }

  const sendMessageToEmacs = (command: string, data: {}) => {
    webSocket.send(JSON.stringify({ command: command, data: data }))
  }
  const openNodeInEmacs = (node: OrgRoamNode) => {
    sendMessageToEmacs('open', { id: node.id })
  }

  const deleteNodeInEmacs = (node: OrgRoamNode) => {
    if (node.level !== 0) {
      return
    }
    sendMessageToEmacs('delete', { id: node.id, file: node.file })
  }

  const createNodeInEmacs = (node: OrgRoamNode) => {
    sendMessageToEmacs('create', { id: node.id, title: node.title, ref: node.properties.ROAM_REFS })
  }

  const handleClick = (click: string, node: OrgRoamNode) => {
    switch (click) {
      //mouse.highlight:
      case mouse.local: {
        handleLocal(node, behavior.localSame)
        break
      }
      case mouse.follow: {
        openNodeInEmacs(node)
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
    setHoverNode(nodeById[emacsNodeId] as NodeObject)
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

  const hiddenNodeIdsRef = useRef<NodeById>({})
  const filteredLinksByNodeId = useRef<LinksByNodeId>({})
  const filteredGraphData = useMemo(() => {
    hiddenNodeIdsRef.current = {}
    const filteredNodes = graphData?.nodes
      ?.filter((nodeArg) => {
        //sometimes there will be some undefined nodes in the mix
        // should probably fix the actual issue, but this is a fix
        if (!nodeArg) {
          return
        }
        const node = nodeArg as OrgRoamNode
        if (
          filter.tagsBlacklist.length &&
          filter.tagsBlacklist.some((tag) => node.tags.indexOf(tag) > -1)
        ) {
          hiddenNodeIdsRef.current = { ...hiddenNodeIdsRef.current, [node.id]: node }
          return false
        }
        if (
          filter.tagsWhitelist.length > 0 &&
          !filter.tagsWhitelist.some((tag) => node.tags.indexOf(tag) > -1)
        ) {
          hiddenNodeIdsRef.current = { ...hiddenNodeIdsRef.current, [node.id]: node }
          return false
        }
        if (filter.filelessCites && node.properties.FILELESS) {
          hiddenNodeIdsRef.current = { ...hiddenNodeIdsRef.current, [node.id]: node }
          return false
        }
        if (filter.bad && node.properties.bad) {
          hiddenNodeIdsRef.current = { ...hiddenNodeIdsRef.current, [node.id]: node }
          return false
        }
        return true
      })
      .filter((node) => {
        const links = linksByNodeId[node?.id as string] ?? []
        const unhiddenLinks = links.filter(
          (link) =>
            !hiddenNodeIdsRef.current[link.source] && !hiddenNodeIdsRef.current[link.target],
        )

        if (!filter.orphans) {
          return true
        }

        if (filter.parents) {
          return unhiddenLinks.length !== 0
        }

        if (unhiddenLinks.length === 0) {
          return false
        }

        return unhiddenLinks.some((link) => !['parent'].includes(link.type))
      })

    const filteredNodeIds = filteredNodes.map((node) => node.id as string)
    const filteredLinks = graphData.links.filter((link) => {
      const [sourceId, targetId] = normalizeLinkEnds(link)
      if (
        filter.bad ||
        filter.tagsBlacklist.length ||
        filter.tagsWhitelist.length ||
        filter.filelessCites
      ) {
        return (
          filteredNodeIds.includes(sourceId as string) &&
          filteredNodeIds.includes(targetId as string)
        )
      }
      const linkRoam = link as OrgRoamLink
      return filter.parents || linkRoam.type !== 'parent'
    })
    return { filteredNodes, filteredLinks }
  }, [filter, graphData])

  const scopedGraphData = useMemo(() => {
    const scopedNodes = filteredGraphData.filteredNodes.filter((node) => {
      const links = linksByNodeId[node.id as string] ?? []
      return (
        scope.nodeIds.includes(node.id as string) ||
        links.some((link) => {
          return scope.nodeIds.includes(link.source) || scope.nodeIds.includes(link.target)
        })
      )
    })

    const scopedNodeIds = scopedNodes.map((node) => node.id as string)

    const scopedLinks = filteredGraphData.filteredLinks.filter((link) => {
      // we need to cover both because force-graph modifies the original data
      // but if we supply the original data on each render, the graph will re-render sporadically
      const [sourceId, targetId] = normalizeLinkEnds(link)
      return (
        scopedNodeIds.includes(sourceId as string) && scopedNodeIds.includes(targetId as string)
      )
    })

    return scope.nodeIds.length === 0
      ? { nodes: filteredGraphData.filteredNodes, links: filteredGraphData.filteredLinks }
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
  }, [physics])

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

  const getThemeColor = (name: string) => {
    if (!theme) {
      return
    }
    return name.split('.').reduce((o, i) => o[i], theme.colors)
  }

  const highlightColors = useMemo(() => {
    return Object.fromEntries(
      colorList.map((color) => {
        const color1 = getThemeColor(color)
        const crisscross = colorList.map((color2) => [
          color2,
          d3int.interpolate(color1, getThemeColor(color2)),
        ])
        return [color, Object.fromEntries(crisscross)]
      }),
    )
  }, [emacsTheme])

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
      ? linksByNodeId[id!]?.filter((link) => link.type === 'parent').length
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
      return highlightColors[nodeColor][visuals.backgroundColor](visuals.highlightFade * opacity)
    }

    if (!needsHighlighting) {
      return highlightColors[visuals.linkColorScheme][visuals.backgroundColor](
        visuals.highlightFade * opacity,
      )
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
    const isHighlightingHappening = !!highlightedNodes.length
    const needsHighlighting = highlightedNodes[node.id!] || previouslyHighlightedNodes[node.id!]
    // if we are matching the node color and don't have a highlight color
    // or we don't have our own scheme and we're not being highlighted
    if (visuals.emacsNodeColor && node.id === emacsNodeId) {
      return getThemeColor(visuals.emacsNodeColor)
    }
    if (tagColors && node.tags.some((tag) => tagColors[tag])) {
      const tagColor = tagColors[node.tags.filter((tag) => tagColors[tag])[0]]
      return highlightColors[tagColor][visuals.backgroundColor](visuals.highlightFade * opacity)
    }
    if (visuals.citeNodeColor && node.properties.ROAM_REFS && node.properties.FILELESS) {
      return needsHighlighting
        ? getThemeColor(visuals.citeNodeColor)
        : highlightColors[visuals.citeNodeColor][visuals.backgroundColor](
            visuals.highlightFade * opacity,
          )
    }
    if (visuals.refNodeColor && node.properties.ROAM_REFS) {
      return needsHighlighting
        ? getThemeColor(visuals.refNodeColor)
        : highlightColors[visuals.refNodeColor][visuals.backgroundColor](
            visuals.highlightFade * opacity,
          )
    }
    if (!needsHighlighting) {
      return highlightColors[getNodeColorById(node.id as string)][visuals.backgroundColor](
        visuals.highlightFade * opacity,
      )
    }
    if (!visuals.nodeHighlight) {
      return getThemeColor(getNodeColorById(node.id as string))
    }
    return highlightColors[getNodeColorById(node.id as string)][visuals.nodeHighlight](opacity)
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
    [visuals.labelTextColor, emacsTheme],
  )
  const labelBackgroundColor = useMemo(
    () => getThemeColor(visuals.labelBackgroundColor),
    [visuals.labelBackgroundColor, emacsTheme],
  )

  const nodeSize = (node: NodeObject) => {
    const links = linksByNodeId[node.id!] ?? []
    const parentNeighbors = links.length ? links.filter((link) => link.type === 'parent').length : 0
    const basicSize =
      3 + links.length * visuals.nodeSizeLinks - (!filter.parents ? parentNeighbors : 0)
    if (visuals.highlightNodeSize === 1) {
      return basicSize
    }
    const highlightSize =
      highlightedNodes[node.id!] || previouslyHighlightedNodes[node.id!]
        ? 1 + opacity * (visuals.highlightNodeSize - 1)
        : 1
    return basicSize * highlightSize
  }

  const [dragging, setDragging] = useState(false)
  const { isOpen, onOpen, onClose } = useDisclosure()
  const [rightClickedNode, setRightClickedNode] = useState<OrgRoamNode | null>(null)
  const [contextPos, setContextPos] = useState([0, 0])
  const openContextMenu = (node: OrgRoamNode, event: any) => {
    setContextPos([event.pageX, event.pageY])
    setRightClickedNode(node)
    onOpen()
  }

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
      return nodeSize(node)
    },
    nodeCanvasObject: (node, ctx, globalScale) => {
      if (!node) {
        return
      }
      if (dragging) {
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
      const label =
        nodeTitle.length > visuals.labelLength
          ? nodeTitle.substring(0, visuals.labelLength) + '...'
          : nodeTitle
      // const label = 'label'
      const fontSize = visuals.labelFontSize / (0.75 * Math.min(Math.max(0.5, globalScale), 3))
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
          : 1 * fadeFactor * (-1 * (visuals.highlightFade * opacity - 1))
      }
      const nodeS = 8 * Math.cbrt(nodeSize(node) * visuals.nodeRel)
      if (visuals.labelBackgroundColor && visuals.labelBackgroundOpacity) {
        const backgroundOpacity = getLabelOpacity() * visuals.labelBackgroundOpacity
        const labelBackground = hexToRGBA(labelBackgroundColor, backgroundOpacity)
        ctx.fillStyle = labelBackground
        ctx.fillRect(
          node.x! - bckgDimensions[0] / 2,
          node.y! - bckgDimensions[1] / 2 + nodeS,
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
      ctx.fillText(label, node.x!, node.y! + nodeS)
    },
    nodeCanvasObjectMode: () => 'after',

    linkDirectionalParticles: visuals.particles ? visuals.particlesNumber : undefined,
    linkDirectionalArrowLength: visuals.arrows ? visuals.arrowsLength : undefined,
    linkDirectionalArrowRelPos: visuals.arrowsPos,
    linkDirectionalArrowColor: visuals.arrowsColor
      ? (link) => getThemeColor(visuals.arrowsColor)
      : undefined,
    linkColor: (link) => {
      const sourceId = typeof link.source === 'object' ? link.source.id! : (link.source as string)
      const targetId = typeof link.target === 'object' ? link.target.id! : (link.target as string)
      const linkIsHighlighted = isLinkRelatedToNode(link, centralHighlightedNode.current)
      const linkWasHighlighted = isLinkRelatedToNode(link, lastHoverNode.current)
      const needsHighlighting = linkIsHighlighted || linkWasHighlighted
      const roamLink = link as OrgRoamLink

      if (visuals.refLinkColor && roamLink.type === 'ref') {
        return needsHighlighting && (visuals.refLinkHighlightColor || visuals.linkHighlight)
          ? highlightColors[visuals.refLinkColor][
              visuals.refLinkHighlightColor || visuals.linkHighlight
            ](opacity)
          : highlightColors[visuals.refLinkColor][visuals.backgroundColor](
              visuals.highlightFade * opacity,
            )
      }
      if (visuals.citeLinkColor && roamLink.type === 'cite') {
        return needsHighlighting && (visuals.citeLinkHighlightColor || visuals.linkHighlight)
          ? highlightColors[visuals.citeLinkColor][
              visuals.citeLinkHighlightColor || visuals.linkHighlight
            ](opacity)
          : highlightColors[visuals.citeLinkColor][visuals.backgroundColor](
              visuals.highlightFade * opacity,
            )
      }

      return getLinkColor(sourceId as string, targetId as string, needsHighlighting)
    },
    linkWidth: (link) => {
      if (visuals.highlightLinkSize === 1) {
        return visuals.linkWidth
      }
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

    onNodeClick: (nodeArg: NodeObject, event: any) => {
      const node = nodeArg as OrgRoamNode
      onClose()
      const isDoubleClick = event.timeStamp - lastNodeClickRef.current < 400
      lastNodeClickRef.current = event.timeStamp
      if (isDoubleClick) {
        return handleClick('double', node)
      }
      return handleClick('click', node)
    },
    onBackgroundClick: () => {
      onClose()
      setHoverNode(null)
      if (scope.nodeIds.length === 0) {
        return
      }
      setScope((currentScope: Scope) => ({
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
    onNodeRightClick: (nodeArg, event) => {
      const node = nodeArg as OrgRoamNode
      openContextMenu(node, event)

      //handleClick('right', node)
    },
    onNodeDrag: (node) => {
      onClose()
      setHoverNode(node)
      setDragging(true)
    },
    onNodeDragEnd: (node) => {
      setHoverNode(null)
      setDragging(false)
    },
  }

  return (
    <Box overflow="hidden">
      {isOpen && (
        <ContextMenu
          scope={scope}
          node={rightClickedNode!}
          nodeType={rightClickedNode?.id}
          background={false}
          coordinates={contextPos}
          handleLocal={handleLocal}
          menuClose={onClose}
          openNodeInEmacs={openNodeInEmacs}
          deleteNodeInEmacs={deleteNodeInEmacs}
          createNodeInEmacs={createNodeInEmacs}
        />
      )}
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
        <ForceGraph2D
          ref={graphRef}
          {...graphCommonProps}
          linkLineDash={(link) => {
            const linkArg = link as OrgRoamLink
            if (visuals.citeDashes && linkArg.type === 'cite') {
              return [visuals.citeDashLength, visuals.citeGapLength]
            }
            if (visuals.refDashes && linkArg.type == 'ref') {
              return [visuals.refDashLength, visuals.refGapLength]
            }
            return null
          }}
        />
      )}
    </Box>
  )
})

function isLinkRelatedToNode(link: LinkObject, node: NodeObject | null) {
  return (
    (link.source as NodeObject)?.id! === node?.id! || (link.target as NodeObject)?.id! === node?.id!
  )
}

function numberWithinRange(num: number, min: number, max: number) {
  return Math.min(Math.max(num, min), max)
}
