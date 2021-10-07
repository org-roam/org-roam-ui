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
import { Tweaks } from '../components/Tweaks'
import { ContextMenu } from '../components/contextmenu'

import { ThemeContext, ThemeContextProps } from '../util/themecontext'
import SpriteText from 'three-spritetext'
import wrap from 'word-wrap'
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

export function GraphPage() {
  const [threeDim, setThreeDim] = usePersistantState('3d', false)
  const [tagColors, setTagColors] = usePersistantState<TagColors>('tagCols', {})
  const [scope, setScope] = useState<Scope>({ nodeIds: [] })

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
  const variablesRef = useRef<{ [variable: string]: string }>({})

  const currentGraphDataRef = useRef<GraphData>({ nodes: [], links: [] })

  const updateGraphData = (orgRoamGraphData: OrgRoamGraphReponse) => {
    const oldNodeById = nodeByIdRef.current
    tagsRef.current = orgRoamGraphData.tags ?? []
    const importNodes = orgRoamGraphData.nodes ?? []
    const importLinks = orgRoamGraphData.links ?? []
    const nodesByFile = importNodes.reduce<NodesByFile>((acc, node) => {
      return {
        ...acc,
        [node.file]: [...(acc[node.file] ?? []), node],
      }
    }, {})

    const headingLinks: OrgRoamLink[] = Object.keys(nodesByFile).flatMap((file) => {
      const nodesInFile = nodesByFile[file] ?? []
      // "file node" as opposed to "heading node"
      const fileNode = nodesInFile.find((node) => node.level === 0)
      const headingNodes = nodesInFile.filter((node) => node.level !== 0)

      if (!fileNode) {
        return []
      }
      return headingNodes.map((headingNode) => {
        const smallerHeadings = nodesInFile.filter((node) => {
          if (
            node.level >= headingNode.level ||
            node.pos >= headingNode.pos ||
            !headingNode.olp?.includes(node.title) //||
            //  node.level >= headingNode.level - headingNode.olp.reverse().indexOf(node.title)
          ) {
            return false
          }
          return true
        })

        // get the nearest heading
        const target = smallerHeadings.reduce((acc, node) => {
          if (node.level > acc.level) {
            acc = node
          }
          return acc
        }, fileNode)

        return {
          source: headingNode.id,
          target: target?.id || fileNode.id,
          type: 'heading',
        }
      })
    })

    // we want to support both linking to only the file node and to the next heading
    // to do this we need both links, as we can't really toggle between them without
    // recalculating the entire graph otherwise
    const fileLinks: OrgRoamLink[] = Object.keys(nodesByFile).flatMap((file) => {
      const nodesInFile = nodesByFile[file] ?? []
      // "file node" as opposed to "heading node"
      const fileNode = nodesInFile.find((node) => node.level === 0)
      const headingNodes = nodesInFile.filter((node) => node.level !== 0)

      if (!fileNode) {
        return []
      }
      return headingNodes.map((headingNode) => {
        return {
          source: headingNode.id,
          target: fileNode.id,
          type: 'parent',
        }
      })
    })

    nodeByIdRef.current = Object.fromEntries(importNodes.map((node) => [node.id, node]))
    const dirtyLinks = [...importLinks, ...headingLinks, ...fileLinks]
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
          pos: 0,
          olp: null,
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
          pos: 0,
          olp: null,
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

    const nodes = [...importNodes, ...nonExistantNodes]
    const orgRoamGraphDataProcessed = {
      nodes,
      links,
    }

    const currentGraphData = currentGraphDataRef.current
    if (currentGraphData.nodes.length === 0) {
      // react-force-graph modifies the graph data implicitly,
      // so we make sure there's no overlap between the objects we pass it and
      // nodeByIdRef, linksByNodeIdRef
      const orgRoamGraphDataClone = JSON.parse(JSON.stringify(orgRoamGraphDataProcessed))
      currentGraphDataRef.current = orgRoamGraphDataClone
      setGraphData(orgRoamGraphDataClone)
      return
    }

    const newNodes = [
      ...currentGraphData.nodes.flatMap((node: NodeObject) => {
        const newNode = nodeByIdRef.current[node?.id!] ?? false
        if (!newNode) {
          return []
        }
        return [{ ...node, ...newNode }]
      }),
      ...Object.keys(nodeByIdRef.current)
        .filter((id) => !oldNodeById[id])
        .map((id) => {
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

    const newerLinks = links.map((link) => {
      const [source, target] = normalizeLinkEnds(link)
      return {
        ...link,
        source: newNodes[nodeIndex![source]],
        target: newNodes[nodeIndex![target]],
      }
    })

    setGraphData({ nodes: newNodes as NodeObject[], links: newerLinks })
  }
  useEffect(() => {
    if (!graphData) {
      return
    }
    currentGraphDataRef.current = graphData
  }, [graphData])

  const { setEmacsTheme } = useContext(ThemeContext)

  const scopeRef = useRef<Scope>({ nodeIds: [] })
  const behaviorRef = useRef(initialBehavior)
  behaviorRef.current = behavior
  const WebSocketRef = useRef<ReconnectingWebSocket | null>(null)

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
      if (sr.nodeIds.length) {
        setScope({ nodeIds: [] })
      }
      setTimeout(
        () => fg.zoomToFit(speed, padding, (node: NodeObject) => nodes[node.id as string]),
        50,
      )
      return
    }
    if (!sr.nodeIds.length) {
      setScope({ nodeIds: [emacsNode] })
      setTimeout(() => {
        fg.centerAt(0, 0, 10)
        fg.zoomToFit(1, padding)
      }, 50)
      return
    }
    if (bh.localSame !== 'add') {
      setScope({ nodeIds: [emacsNode] })
      setTimeout(() => {
        fg.centerAt(0, 0, 10)
        fg.zoomToFit(1, padding)
      }, 50)
      return
    }

    // if the node is in the scoped nodes, add it to scope instead of replacing it
    if (
      !sr.nodeIds.includes(emacsNode) ||
      !sr.nodeIds.some((scopeId: string) => {
        return nodes[scopeId]
      })
    ) {
      setScope({ nodeIds: [emacsNode] })
      setTimeout(() => {
        fg.centerAt(0, 0, 10)
        fg.zoomToFit(1, padding)
      }, 50)
      return
    }
    setScope((currentScope: Scope) => ({
      ...currentScope,
      nodeIds: [...currentScope.nodeIds, emacsNode as string],
    }))
    setTimeout(() => {
      fg.centerAt(0, 0, 10)
      fg.zoomToFit(1, padding)
    }, 50)
  }

  useEffect(() => {
    // initialize websocket
    WebSocketRef.current = new ReconnectingWebSocket('ws://localhost:35903')
    WebSocketRef.current.addEventListener('open', () => {
      console.log('Connection with Emacs established')
    })
    WebSocketRef.current.addEventListener('message', (event: any) => {
      const bh = behaviorRef.current
      const message = JSON.parse(event.data)
      switch (message.type) {
        case 'graphdata':
          return updateGraphData(message.data)
        case 'variables':
          variablesRef.current = message.data
          return
        case 'theme':
          return setEmacsTheme(['custom', message.data])
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

  useEffect(() => {
    const fg = graphRef.current
    if (!fg || scope.nodeIds.length > 1) {
      return
    }
    if (!scope.nodeIds.length && physics.gravityOn) {
      fg.zoomToFit()
      return
    }
    setTimeout(() => {
      fg.zoomToFit(5, 200)
    }, 50)
  }, [scope.nodeIds])

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
          variables={variablesRef.current}
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
  variables: { [variable: string]: string }
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
    variables,
  } = props

  const { dailyDir, roamDir } = variables
  // react-force-graph does not track window size
  // https://github.com/vasturiano/react-force-graph/issues/233
  // does not work below a certain width
  const [windowWidth, windowHeight] = useWindowSize()

  const [hoverNode, setHoverNode] = useState<NodeObject | null>(null)

  const [rightClickedNode, setRightClickedNode] = useState<OrgRoamNode | null>(null)
  const [contextPos, setContextPos] = useState([0, 0])

  const theme = useTheme()

  const { emacsTheme } = useContext<ThemeContextProps>(ThemeContext)

  const handleLocal = (node: OrgRoamNode, add: string) => {
    if (add === 'replace') {
      setScope({ nodeIds: [node.id] })
      return
    }
    if (scope.nodeIds.includes(node.id as string)) {
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

  const contextMenu = useDisclosure()

  const openContextMenu = (node: OrgRoamNode, event: any) => {
    setContextPos([event.pageX, event.pageY])
    setRightClickedNode(node)
    contextMenu.onOpen()
  }

  const handleClick = (click: string, node: OrgRoamNode, event: any) => {
    switch (click) {
      case mouse.local: {
        handleLocal(node, behavior.localSame)
        break
      }
      case mouse.follow: {
        openNodeInEmacs(node)
        break
      }
      case mouse.context: {
        openContextMenu(node, event)
      }
      default:
        break
    }
  }

  const findNthNeighbors = (ids: string[], n: number) => {
    let queue = [ids[0]]
    let todo: string[] = []
    const completed = [ids[0]]
    Array.from({ length: n }, () => {
      queue.forEach((node) => {
        const links = filteredLinksByNodeIdRef.current[node as string] ?? []
        links.forEach((link) => {
          const [sourceId, targetId] = normalizeLinkEnds(link)
          if (!completed.includes(sourceId)) {
            todo.push(sourceId)
            return
          }
          if (!completed.includes(targetId)) {
            todo.push(targetId)
            return
          }
          return
        })
      })
      queue = todo
      todo.forEach((neighbor) => neighbor && completed.push(neighbor))
      todo = []
    })
    return completed
  }

  const centralHighlightedNode = useRef<NodeObject | null>(null)

  useEffect(() => {
    if (!emacsNodeId) {
      return
    }
    setHoverNode(nodeById[emacsNodeId] as NodeObject)
  }, [emacsNodeId])

  const filteredLinksByNodeIdRef = useRef<LinksByNodeId>({})

  const hiddenNodeIdsRef = useRef<NodeById>({})
  const filteredGraphData = useMemo(() => {
    hiddenNodeIdsRef.current = {}
    const filteredNodes = graphData?.nodes
      ?.filter((nodeArg) => {
        const node = nodeArg as OrgRoamNode
        if (
          filter.tagsBlacklist.length &&
          filter.tagsBlacklist.some((tag) => node?.tags?.indexOf(tag) > -1)
        ) {
          hiddenNodeIdsRef.current = { ...hiddenNodeIdsRef.current, [node.id]: node }
          return false
        }
        if (
          filter.tagsWhitelist.length > 0 &&
          !filter.tagsWhitelist.some((tag) => node?.tags?.indexOf(tag) > -1)
        ) {
          hiddenNodeIdsRef.current = { ...hiddenNodeIdsRef.current, [node.id]: node }
          return false
        }
        if (filter.filelessCites && node?.properties?.FILELESS) {
          hiddenNodeIdsRef.current = { ...hiddenNodeIdsRef.current, [node.id]: node }
          return false
        }
        if (filter.bad && node.properties.bad) {
          hiddenNodeIdsRef.current = { ...hiddenNodeIdsRef.current, [node.id]: node }
          return false
        }

        if (filter.dailies && dailyDir && node.file?.includes(dailyDir)) {
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

        if (filter.parent) {
          return unhiddenLinks.length !== 0
        }

        if (unhiddenLinks.length === 0) {
          return false
        }

        return unhiddenLinks.some((link) => !['parent', 'heading'].includes(link.type))
      })

    const filteredNodeIds = filteredNodes.map((node) => node.id as string)
    const filteredLinks = graphData.links.filter((link) => {
      const [sourceId, targetId] = normalizeLinkEnds(link)
      if (
        !filteredNodeIds.includes(sourceId as string) ||
        !filteredNodeIds.includes(targetId as string)
      ) {
        return false
      }
      const linkRoam = link as OrgRoamLink
      if (!filter.parent) {
        return !['parent', 'heading'].includes(linkRoam.type)
      }
      if (filter.parent === 'heading') {
        return linkRoam.type !== 'parent'
      }
      return linkRoam.type !== 'heading'
    })

    filteredLinksByNodeIdRef.current = filteredLinks.reduce<LinksByNodeId>((acc, linkArg) => {
      const link = linkArg as OrgRoamLink
      const [sourceId, targetId] = normalizeLinkEnds(link)
      return {
        ...acc,
        [sourceId]: [...(acc[sourceId] ?? []), link],
        [targetId]: [...(acc[targetId] ?? []), link],
      }
    }, {})

    return { nodes: filteredNodes, links: filteredLinks }
  }, [filter, graphData])

  const [scopedGraphData, setScopedGraphData] = useState<GraphData>({ nodes: [], links: [] })

  useEffect(() => {
    if (!scope.nodeIds.length) {
      return
    }
    const oldScopedNodes = scope.nodeIds.length > 1 ? scopedGraphData.nodes : []
    const oldScopedNodeIds = oldScopedNodes.map((node) => node.id as string)
    const neighbs = findNthNeighbors(scope.nodeIds, 1)
    const newScopedNodes = filteredGraphData.nodes
      .filter((node) => {
        if (oldScopedNodes.length) {
          if (oldScopedNodeIds.includes(node.id as string)) {
            return false
          }
          const links = filteredLinksByNodeIdRef.current[node.id as string] ?? []
          return links.some((link) => {
            const [source, target] = normalizeLinkEnds(link)
            return scope.nodeIds.includes(source) || scope.nodeIds.includes(target)
          })
        }
        return neighbs.includes(node.id as string)
        // this creates new nodes, to separate them from the nodes in the global graph
        // and positions them in the center, so that the camera is not so jumpy
      })
      .map((node) => {
        return { ...node, x: 0, y: 0, vy: 0, vx: 0 }
      })
    const scopedNodes = [...oldScopedNodes, ...newScopedNodes]
    const scopedNodeIds = scopedNodes.map((node) => node.id as string)

    const oldScopedLinks = scope.nodeIds.length > 1 ? scopedGraphData.links : []
    const newScopedLinks = filteredGraphData.links
      .filter((link) => {
        // we need to cover both because force-graph modifies the original data
        // but if we supply the original data on each render, the graph will re-render sporadically
        const [sourceId, targetId] = normalizeLinkEnds(link)
        if (
          oldScopedLinks.length &&
          oldScopedNodeIds.includes(targetId) &&
          oldScopedNodeIds.includes(sourceId)
        ) {
          return false
        }
        return (
          scopedNodeIds.includes(sourceId as string) && scopedNodeIds.includes(targetId as string)
        )
      })
      .map((link) => {
        const [sourceId, targetId] = normalizeLinkEnds(link)
        return { source: sourceId, target: targetId }
      })

    const scopedLinks = [...oldScopedLinks, ...newScopedLinks]

    setScopedGraphData({ nodes: scopedNodes, links: scopedLinks })
  }, [filter, scope, JSON.stringify(graphData), filteredGraphData.links, filteredGraphData.nodes])

  centralHighlightedNode.current = hoverNode
  const highlightedNodes = useMemo(() => {
    if (!centralHighlightedNode.current) {
      return {}
    }

    const links = filteredLinksByNodeIdRef.current[centralHighlightedNode.current.id!]
    if (!links) {
      return {}
    }
    return Object.fromEntries(
      [
        centralHighlightedNode.current.id! as string,
        ...links.flatMap((link) => [link.source, link.target]),
      ].map((nodeId) => [nodeId, {}]),
    )
  }, [centralHighlightedNode.current, filteredLinksByNodeIdRef.current])

  useEffect(() => {
    ;(async () => {
      const fg = graphRef.current
      const d3 = await d3promise
      if (physics.gravityOn && !(scope.nodeIds.length && !physics.gravityLocal)) {
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
  }, [physics, threeDim, scope])

  // Normally the graph doesn't update when you just change the physics parameters
  // This forces the graph to make a small update when you do
  useEffect(() => {
    graphRef.current?.d3ReheatSimulation()
  }, [physics, scope.nodeIds.length])

  // shitty handler to check for doubleClicks
  const lastNodeClickRef = useRef(0)

  const [opacity, setOpacity] = useState(1)
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

  // FIXME: Somehow the "linksByNodeId" call causes parent nodes to be always highlighted
  // Replacing this with "linksByNodeIdRef.current" should solve this, but instead leads to no
  // highlighting whatsoever.
  const previouslyHighlightedNodes = useMemo(() => {
    const previouslyHighlightedLinks =
      filteredLinksByNodeIdRef.current[lastHoverNode.current?.id!] ?? []
    return Object.fromEntries(
      [
        lastHoverNode.current?.id! as string,
        ...previouslyHighlightedLinks.flatMap((link) => normalizeLinkEnds(link)),
      ].map((nodeId) => [nodeId, {}]),
    )
  }, [JSON.stringify(hoverNode), lastHoverNode.current, filteredLinksByNodeIdRef.current])

  const getNodeColorById = (id: string) => {
    const linklen = filteredLinksByNodeIdRef.current[id!]?.length ?? 0
    /* const parentCiteNeighbors = linklen
     *   ? linksByNodeId[id!]?.filter((link) => ['parent', 'heading', 'cite', 'ref'].includes(link.type)).length
     *   : 0
     * const neighbors = filter.parent ? linklen : linklen - parentCiteNeighbors! */

    return visuals.nodeColorScheme[
      numberWithinRange(linklen, 0, visuals.nodeColorScheme.length - 1)
    ]
  }
  const getLinkNodeColor = (sourceId: string, targetId: string) => {
    return filteredLinksByNodeIdRef.current[sourceId]!.length >
      filteredLinksByNodeIdRef.current[targetId]!.length
      ? getNodeColorById(sourceId)
      : getNodeColorById(targetId)
  }

  const getLinkColor = (sourceId: string, targetId: string, needsHighlighting: boolean) => {
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

  const labelTextColor = useMemo(
    () => getThemeColor(visuals.labelTextColor),
    [visuals.labelTextColor, emacsTheme],
  )
  const labelBackgroundColor = useMemo(
    () => getThemeColor(visuals.labelBackgroundColor),
    [visuals.labelBackgroundColor, emacsTheme],
  )

  const nodeSize = (node: NodeObject) => {
    const links = filteredLinksByNodeIdRef.current[node.id!] ?? []
    const parentNeighbors = links.length ? links.filter((link) => link.type === 'parent').length : 0
    const basicSize =
      3 + links.length * visuals.nodeSizeLinks - (!filter.parent ? parentNeighbors : 0)
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

  const [zoom, setZoom] = useState(1)
  const graphCommonProps: ComponentPropsWithoutRef<typeof TForceGraph2D> = {
    graphData: scope.nodeIds.length ? scopedGraphData : filteredGraphData,
    width: windowWidth,
    height: windowHeight,
    backgroundColor: theme.colors.gray[visuals.backgroundColor],
    warmupTicks: scope.nodeIds.length === 1 ? 100 : scope.nodeIds.length > 1 ? 20 : 0,
    onZoom: ({ k, x, y }) => setZoom(k),
    nodeLabel: (node) => (node as OrgRoamNode).title,
    nodeColor: (node) => {
      return getNodeColor(node as OrgRoamNode)
    },
    nodeRelSize: visuals.nodeRel,
    nodeVal: (node) => {
      return nodeSize(node) / Math.pow(zoom, visuals.nodeZoomSize)
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
      const label = nodeTitle.substring(0, visuals.labelLength)
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
      const wordsArray = wrap(label, { width: visuals.labelWordWrap }).split('\n')

      const truncatedWords =
        nodeTitle.length > visuals.labelLength
          ? [...wordsArray.slice(0, -1), `${wordsArray.slice(-1)}...`]
          : wordsArray
      truncatedWords.forEach((word, index) => {
        ctx.fillText(word, node.x!, node.y! + nodeS + visuals.labelLineSpace * fontSize * index)
      })
    },
    nodeCanvasObjectMode: () => 'after',

    linkDirectionalParticles: visuals.particles ? visuals.particlesNumber : undefined,
    linkDirectionalArrowLength: visuals.arrows ? visuals.arrowsLength : undefined,
    linkDirectionalArrowRelPos: visuals.arrowsPos,
    linkDirectionalArrowColor: visuals.arrowsColor
      ? () => getThemeColor(visuals.arrowsColor)
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
      if (visuals.citeLinkColor && roamLink.type?.includes('cite')) {
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
      contextMenu.onClose()
      const doubleClickTimeBuffer = 200
      const isDoubleClick = event.timeStamp - lastNodeClickRef.current < doubleClickTimeBuffer
      lastNodeClickRef.current = event.timeStamp
      if (isDoubleClick) {
        return handleClick('double', node, event)
      }

      const prevNodeClickTime = lastNodeClickRef.current
      return setTimeout(() => {
        if (lastNodeClickRef.current !== prevNodeClickTime) {
          return
        }
        return handleClick('click', node, event)
      }, doubleClickTimeBuffer)
    },
    onBackgroundClick: () => {
      contextMenu.onClose()
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

      handleClick('right', node, event)
    },
    onNodeDrag: (node) => {
      contextMenu.onClose()
      setHoverNode(node)
      setDragging(true)
    },
    onNodeDragEnd: () => {
      setHoverNode(null)
      setDragging(false)
    },
  }

  return (
    <Box overflow="hidden">
      {contextMenu.isOpen && (
        <ContextMenu
          scope={scope}
          node={rightClickedNode!}
          nodeType={rightClickedNode?.id}
          background={false}
          coordinates={contextPos}
          handleLocal={handleLocal}
          menuClose={contextMenu.onClose.bind(contextMenu)}
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
            if (visuals.citeDashes && linkArg.type?.includes('cite')) {
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

function normalizeLinkEnds(link: OrgRoamLink | LinkObject): [string, string] {
  // we need to cover both because force-graph modifies the original data
  // but if we supply the original data on each render, the graph will re-render sporadically
  const sourceId =
    typeof link.source === 'object' ? (link.source.id! as string) : (link.source as string)
  const targetId =
    typeof link.target === 'object' ? (link.target.id! as string) : (link.target as string)
  return [sourceId, targetId]
}

function hexToRGBA(hex: string, opacity: number) {
  return (
    'rgba(' +
    (hex = hex.replace('#', ''))
      .match(new RegExp('(.{' + hex.length / 3 + '})', 'g'))!
      .map(function (l) {
        return parseInt(hex.length % 2 ? l + l : l, 16)
      })
      .concat(isFinite(opacity) ? opacity : 1)
      .join(',') +
    ')'
  )
}
