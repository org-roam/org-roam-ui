import { HamburgerIcon } from '@chakra-ui/icons'
import {
  Box,
  Flex,
  Heading,
  IconButton,
  Slide,
  Tooltip,
  useDisclosure,
  useOutsideClick,
  useTheme,
} from '@chakra-ui/react'
import { useAnimation } from '@lilib/hooks'
import { useWindowSize, useWindowWidth } from '@react-hook/window-size'
import * as d3int from 'd3-interpolate'
import { GraphData, LinkObject, NodeObject } from 'force-graph'
import Head from 'next/head'
import React, {
  ComponentPropsWithoutRef,
  forwardRef,
  useContext,
  useEffect,
  useMemo,
  useRef,
  useState,
} from 'react'
//@ts-expect-error
import jLouvain from 'jlouvain.js'
import type {
  ForceGraph2D as TForceGraph2D,
  ForceGraph3D as TForceGraph3D,
} from 'react-force-graph'
import { BiChart, BiNetworkChart } from 'react-icons/bi'
import { BsReverseLayoutSidebarInsetReverse } from 'react-icons/bs'
import ReconnectingWebSocket from 'reconnecting-websocket'
import SpriteText from 'three-spritetext'
import useUndo from 'use-undo'
import wrap from 'word-wrap'
import { OrgRoamGraphReponse, OrgRoamLink, OrgRoamNode } from '../api'
import {
  algos,
  colorList,
  initialBehavior,
  initialColoring,
  initialFilter,
  initialLocal,
  initialMouse,
  initialPhysics,
  initialVisuals,
  TagColors,
} from '../components/config'
import { ContextMenu } from '../components/contextmenu'
import Sidebar from '../components/Sidebar'
import { Tweaks } from '../components/Tweaks'
import { usePersistantState } from '../util/persistant-state'
import { ThemeContext, ThemeContextProps } from '../util/themecontext'
import { openNodeInEmacs } from '../util/webSocketFunctions'
import { drawLabels } from '../components/Graph/drawLabels'
import { VariablesContext } from '../util/variablesContext'

const d3promise = import('d3-force-3d')

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
export type NodeByCite = { [key: string]: OrgRoamNode | undefined }
export interface EmacsVariables {
  roamDir?: string
  dailyDir?: string
  katexMacros?: { [key: string]: string }
  attachDir?: string
  subDirs: string[]
}
export type Tags = string[]
export type Scope = {
  nodeIds: string[]
  excludedNodeIds: string[]
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
  return (
    <>
      <Head>
        <title>ORUI</title>
      </Head>
      <GraphPage />
    </>
  )
}

export function GraphPage() {
  const [threeDim, setThreeDim] = usePersistantState('3d', false)
  const [tagColors, setTagColors] = usePersistantState<TagColors>('tagCols', {})
  const [scope, setScope] = useState<Scope>({ nodeIds: [], excludedNodeIds: [] })

  const [physics, setPhysics] = usePersistantState('physics', initialPhysics)
  const [filter, setFilter] = usePersistantState('filter', initialFilter)
  const [visuals, setVisuals] = usePersistantState('visuals', initialVisuals)
  const [graphData, setGraphData] = useState<GraphData | null>(null)
  const [emacsNodeId, setEmacsNodeId] = useState<string | null>(null)
  const [behavior, setBehavior] = usePersistantState('behavior', initialBehavior)
  const [mouse, setMouse] = usePersistantState('mouse', initialMouse)
  const [coloring, setColoring] = usePersistantState('coloring', initialColoring)
  const [local, setLocal] = usePersistantState('local', initialLocal)

  const [
    previewNodeState,
    {
      set: setPreviewNode,
      reset: resetPreviewNode,
      undo: previousPreviewNode,
      redo: nextPreviewNode,
      canUndo,
      canRedo,
    },
  ] = useUndo<NodeObject>({})
  const {
    past: pastPreviewNodes,
    present: previewNode,
    future: futurePreviewNodes,
  } = previewNodeState
  const [sidebarHighlightedNode, setSidebarHighlightedNode] = useState<OrgRoamNode | null>(null)
  const { isOpen, onOpen, onClose } = useDisclosure()

  const nodeByIdRef = useRef<NodeById>({})
  const linksByNodeIdRef = useRef<LinksByNodeId>({})
  const nodeByCiteRef = useRef<NodeByCite>({})
  const tagsRef = useRef<Tags>([])
  const graphRef = useRef<any>(null)
  const [emacsVariables, setEmacsVariables] = useState<EmacsVariables>({} as EmacsVariables)
  const clusterRef = useRef<{ [id: string]: number }>({})

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

    // generate links between level 2 nodes and the level 1 node above it
    // org-roam does not generate such links, so we have to put them in ourselves
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
            !headingNode.olp?.includes((node.title as string)?.replace(/ *\[\d*\/\d*\] */g, ''))
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

    nodeByCiteRef.current = nodes.reduce<NodeByCite>((acc, node) => {
      const ref = node.properties?.ROAM_REFS as string
      if (!ref?.includes('cite')) {
        return acc
      }
      const key = ref.replaceAll(/cite:(.*)/g, '$1')
      if (!key) {
        return acc
      }
      return {
        ...acc,
        [key]: node,
      }
    }, {})

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

  const scopeRef = useRef<Scope>({ nodeIds: [], excludedNodeIds: [] })
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
        setScope({ nodeIds: [], excludedNodeIds: [] })
      }
      setTimeout(
        () => fg.zoomToFit(speed, padding, (node: NodeObject) => nodes[node.id as string]),
        50,
      )
      return
    }
    if (!sr.nodeIds.length) {
      setScope((current: Scope) => ({ ...current, nodeIds: [emacsNode] }))
      setTimeout(() => {
        fg.centerAt(0, 0, 10)
        fg.zoomToFit(1, padding)
      }, 50)
      return
    }
    if (bh.localSame !== 'add') {
      setScope((current: Scope) => ({ ...current, nodeIds: [emacsNode] }))
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
      setScope((current: Scope) => ({ ...current, nodeIds: [emacsNode] }))
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
          setEmacsVariables(message.data)
          console.log(message)
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

  const [windowWidth, windowHeight] = useWindowSize()

  const contextMenuRef = useRef<any>()
  const [contextMenuTarget, setContextMenuTarget] = useState<OrgRoamNode | string | null>(null)
  type ContextPos = {
    left: number | undefined
    right: number | undefined
    top: number | undefined
    bottom: number | undefined
  }
  const [contextPos, setContextPos] = useState<ContextPos>({
    left: 0,
    top: 0,
    right: undefined,
    bottom: undefined,
  })

  const contextMenu = useDisclosure()
  useOutsideClick({
    ref: contextMenuRef,
    handler: () => {
      contextMenu.onClose()
    },
  })

  const openContextMenu = (target: OrgRoamNode | string, event: any, coords?: ContextPos) => {
    coords
      ? setContextPos(coords)
      : setContextPos({ left: event.pageX, top: event.pageY, right: undefined, bottom: undefined })
    setContextMenuTarget(target)
    contextMenu.onOpen()
  }

  const handleLocal = (node: OrgRoamNode, command: string) => {
    if (command === 'remove') {
      setScope((currentScope: Scope) => ({
        ...currentScope,
        excludedNodeIds: [...currentScope.excludedNodeIds, node.id as string],
      }))
      return
    }
    if (command === 'replace') {
      setScope({ nodeIds: [node.id], excludedNodeIds: [] })
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

  const [mainItem, setMainItem] = useState({
    type: 'Graph',
    title: 'Graph',
    icon: <BiNetworkChart />,
  })

  const [mainWindowWidth, setMainWindowWidth] = usePersistantState<number>(
    'mainWindowWidth',
    windowWidth,
  )

  console.log(emacsVariables)
  return (
    <VariablesContext.Provider value={{ ...emacsVariables }}>
      <Box
        display="flex"
        alignItems="flex-start"
        flexDirection="row"
        height="100vh"
        overflow="clip"
      >
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
            coloring,
            setColoring,
            local,
            setLocal,
          }}
          tags={tagsRef.current}
        />
        <Box position="absolute">
          {graphData && (
            <Graph
              //ref={graphRef}
              nodeById={nodeByIdRef.current!}
              linksByNodeId={linksByNodeIdRef.current!}
              webSocket={WebSocketRef.current}
              variables={emacsVariables}
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
                setPreviewNode,
                sidebarHighlightedNode,
                windowWidth,
                windowHeight,
                openContextMenu,
                contextMenu,
                handleLocal,
                mainWindowWidth,
                setMainWindowWidth,
                setContextMenuTarget,
                graphRef,
                clusterRef,
                coloring,
                local,
              }}
            />
          )}
        </Box>
        <Box position="relative" zIndex={4} width="100%">
          <Flex className="headerBar" h={10} flexDir="column">
            <Flex alignItems="center" h={10} justifyContent="flex-end">
              {/* <Flex flexDir="row" alignItems="center">
               *   <Box color="blue.500" bgColor="alt.100" h="100%" p={3} mr={4}>
               *     {mainItem.icon}
               *   </Box>
               *   <Heading size="sm">{mainItem.title}</Heading>
               * </Flex> */}
              <Flex height="100%" flexDirection="row">
                {scope.nodeIds.length > 0 && (
                  <Tooltip label="Return to main graph">
                    <IconButton
                      m={1}
                      icon={<BiNetworkChart />}
                      aria-label="Exit local mode"
                      onClick={() =>
                        setScope((currentScope: Scope) => ({
                          ...currentScope,
                          nodeIds: [],
                        }))
                      }
                      variant="subtle"
                    />
                  </Tooltip>
                )}
                <Tooltip label={isOpen ? 'Close sidebar' : 'Open sidebar'}>
                  <IconButton
                    m={1}
                    // eslint-disable-next-line react/jsx-no-undef
                    icon={<BsReverseLayoutSidebarInsetReverse />}
                    aria-label="Close file-viewer"
                    variant="subtle"
                    onClick={isOpen ? onClose : onOpen}
                  />
                </Tooltip>
              </Flex>
            </Flex>
          </Flex>
        </Box>

        <Box position="relative" zIndex={4}>
          <Sidebar
            {...{
              isOpen,
              onOpen,
              onClose,
              previewNode,
              setPreviewNode,
              canUndo,
              canRedo,
              previousPreviewNode,
              nextPreviewNode,
              resetPreviewNode,
              setSidebarHighlightedNode,
              openContextMenu,
              scope,
              setScope,
              windowWidth,
              tagColors,
              setTagColors,
              filter,
              setFilter,
            }}
            macros={emacsVariables.katexMacros}
            attachDir={emacsVariables.attachDir || ''}
            nodeById={nodeByIdRef.current!}
            linksByNodeId={linksByNodeIdRef.current!}
            nodeByCite={nodeByCiteRef.current!}
          />
        </Box>
        {contextMenu.isOpen && (
          <div ref={contextMenuRef}>
            <ContextMenu
              //contextMenuRef={contextMenuRef}
              scope={scope}
              target={contextMenuTarget}
              background={false}
              coordinates={contextPos}
              handleLocal={handleLocal}
              menuClose={contextMenu.onClose.bind(contextMenu)}
              webSocket={WebSocketRef.current}
              setPreviewNode={setPreviewNode}
              setFilter={setFilter}
              filter={filter}
              setTagColors={setTagColors}
              tagColors={tagColors}
            />
          </div>
        )}
      </Box>
    </VariablesContext.Provider>
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
  local: typeof initialLocal
  scope: Scope
  setScope: any
  webSocket: any
  tagColors: { [tag: string]: string }
  setPreviewNode: any
  sidebarHighlightedNode: OrgRoamNode | null
  windowWidth: number
  windowHeight: number
  setContextMenuTarget: any
  openContextMenu: any
  contextMenu: any
  handleLocal: any
  mainWindowWidth: number
  setMainWindowWidth: any
  variables: EmacsVariables
  graphRef: any
  clusterRef: any
  coloring: typeof initialColoring
}

export const Graph = function (props: GraphProps) {
  const {
    graphRef,
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
    local,
    setScope,
    webSocket,
    tagColors,
    setPreviewNode,
    sidebarHighlightedNode,
    windowWidth,
    windowHeight,
    setContextMenuTarget,
    openContextMenu,
    contextMenu,
    handleLocal,
    variables,
    clusterRef,
    coloring,
  } = props

  const { dailyDir, roamDir } = variables

  const [hoverNode, setHoverNode] = useState<NodeObject | null>(null)

  const theme = useTheme()

  const { emacsTheme } = useContext<ThemeContextProps>(ThemeContext)

  const handleClick = (click: string, node: OrgRoamNode, event: any) => {
    switch (click) {
      case mouse.preview: {
        setPreviewNode(node)
        break
      }
      case mouse.local: {
        handleLocal(node, behavior.localSame)
        break
      }
      case mouse.follow: {
        openNodeInEmacs(node, webSocket)
        break
      }
      case mouse.context: {
        openContextMenu(node, event)
      }
      default:
        break
    }
  }

  const findNthNeighbors = (ids: string[], excludedIds: string[], n: number) => {
    let queue = [ids[0]]
    let todo: string[] = []
    const completed = [ids[0]]
    Array.from({ length: n }, () => {
      queue.forEach((node) => {
        const links = filteredLinksByNodeIdRef.current[node as string] ?? []
        links.forEach((link) => {
          const [sourceId, targetId] = normalizeLinkEnds(link)
          if (excludedIds.some((id) => [sourceId, targetId].includes(id))) {
            return
          }
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
        //dirs
        if (
          filter.dirsBlocklist.length &&
          filter.dirsBlocklist.some((dir) => node?.file?.includes(dir))
        ) {
          hiddenNodeIdsRef.current = { ...hiddenNodeIdsRef.current, [node.id]: node }
          return false
        }
        if (
          filter.dirsAllowlist.length > 0 &&
          !filter.dirsAllowlist.some((dir) => node?.file?.includes(dir))
        ) {
          hiddenNodeIdsRef.current = { ...hiddenNodeIdsRef.current, [node.id]: node }
          return false
        }

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
        if (filter?.bad && node?.properties?.bad) {
          hiddenNodeIdsRef.current = { ...hiddenNodeIdsRef.current, [node.id]: node }
          return false
        }

        if (filter.dailies && dailyDir && node.file?.includes(dailyDir)) {
          hiddenNodeIdsRef.current = { ...hiddenNodeIdsRef.current, [node.id]: node }
          return false
        }
        if (filter.noter && node.properties?.NOTER_PAGE) {
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

    const weightedLinks = filteredLinks.map((l) => {
      const [target, source] = normalizeLinkEnds(l)
      const link = l as OrgRoamLink
      return { target, source, weight: link.type === 'cite' ? 1 : 2 }
    })

    if (coloring.method === 'community') {
      const community = jLouvain().nodes(filteredNodeIds).edges(weightedLinks)
      clusterRef.current = community()
    }
    /* clusterRef.current = Object.fromEntries(
     *   Object.entries(community()).sort(([, a], [, b]) => a - b),
     * ) */
    //console.log(clusterRef.current)
    return { nodes: filteredNodes, links: filteredLinks }
  }, [filter, graphData, coloring.method])

  const [scopedGraphData, setScopedGraphData] = useState<GraphData>({ nodes: [], links: [] })

  useEffect(() => {
    if (!scope.nodeIds.length) {
      return
    }
    const oldScopedNodes =
      scope.nodeIds.length > 1
        ? scopedGraphData.nodes.filter((n) => !scope.excludedNodeIds.includes(n.id as string))
        : []
    const oldScopedNodeIds = oldScopedNodes.map((node) => node.id as string)
    const neighbs = findNthNeighbors(scope.nodeIds, scope.excludedNodeIds, local.neighbors)
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

    const oldRawScopedLinks = scope.nodeIds.length > 1 ? scopedGraphData.links : []
    const oldScopedLinks = oldRawScopedLinks.filter((l) => {
      !scope.excludedNodeIds.some((e) => normalizeLinkEnds(l).includes(e))
    })
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
  }, [
    local.neighbors,
    filter,
    JSON.stringify(scope),
    JSON.stringify(graphData),
    filteredGraphData.links,
    filteredGraphData.nodes,
  ])

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
        centralHighlightedNode.current?.id! as string,
        ...links.flatMap((link) => [link.source, link.target]),
      ].map((nodeId) => [nodeId, {}]),
    )
  }, [
    JSON.stringify(centralHighlightedNode.current),
    JSON.stringify(filteredLinksByNodeIdRef.current),
  ])

  useEffect(() => {
    if (sidebarHighlightedNode?.id) {
      setHoverNode(sidebarHighlightedNode)
    } else {
      setHoverNode(null)
    }
  }, [sidebarHighlightedNode])

  const lastHoverNode = useRef<OrgRoamNode | null>(null)
  useEffect(() => {
    centralHighlightedNode.current = hoverNode
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

  const highlightColors = useMemo(() => {
    return Object.fromEntries(
      colorList.map((color) => {
        const color1 = getThemeColor(color, theme)
        const crisscross = colorList.map((color2) => [
          color2,
          d3int.interpolate(color1, getThemeColor(color2, theme)),
        ])
        return [color, Object.fromEntries(crisscross)]
      }),
    )
  }, [emacsTheme])

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
    if (coloring.method === 'degree') {
      return visuals.nodeColorScheme[
        numberWithinRange(linklen, 0, visuals.nodeColorScheme.length - 1)
      ]
    }
    return visuals.nodeColorScheme[
      linklen && clusterRef.current[id] % visuals.nodeColorScheme.length
    ]
  }

  const getLinkNodeColor = (sourceId: string, targetId: string) => {
    return filteredLinksByNodeIdRef.current[sourceId]!.length >
      filteredLinksByNodeIdRef.current[targetId]!.length
      ? getNodeColorById(sourceId)
      : getNodeColorById(targetId)
  }

  const getLinkColor = (
    sourceId: string,
    targetId: string,
    needsHighlighting: boolean,
    theme: any,
  ) => {
    if (!visuals.linkHighlight && !visuals.linkColorScheme && !needsHighlighting) {
      const nodeColor = getLinkNodeColor(sourceId, targetId)
      return getThemeColor(nodeColor, theme)
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
      return getThemeColor(nodeColor, theme)
    }

    if (!visuals.linkHighlight) {
      return getThemeColor(visuals.linkColorScheme, theme)
    }

    if (!visuals.linkColorScheme) {
      return highlightColors[getLinkNodeColor(sourceId, targetId)][visuals.linkHighlight](opacity)
    }

    return highlightColors[visuals.linkColorScheme][visuals.linkHighlight](opacity)
  }

  const getNodeColor = (node: OrgRoamNode, theme: any) => {
    const needsHighlighting = highlightedNodes[node.id!] || previouslyHighlightedNodes[node.id!]
    //const needsHighlighting = hoverNode?.id === node.id! || lastHoverNode?.current?.id === node.id
    // if we are matching the node color and don't have a highlight color
    // or we don't have our own scheme and we're not being highlighted
    if (visuals.emacsNodeColor && node.id === emacsNodeId) {
      return getThemeColor(visuals.emacsNodeColor, theme)
    }
    if (tagColors && node?.tags.some((tag) => tagColors[tag])) {
      const tagColor = tagColors[node?.tags.filter((tag) => tagColors[tag])[0]]
      return needsHighlighting
        ? highlightColors[tagColor][tagColor](visuals.highlightFade * opacity)
        : highlightColors[tagColor][visuals.backgroundColor](visuals.highlightFade * opacity)
    }
    if (visuals.citeNodeColor && node?.properties?.ROAM_REFS && node?.properties?.FILELESS) {
      return needsHighlighting
        ? getThemeColor(visuals.citeNodeColor, theme)
        : highlightColors[visuals.citeNodeColor][visuals.backgroundColor](
            visuals.highlightFade * opacity,
          )
    }
    if (visuals.refNodeColor && node.properties.ROAM_REFS) {
      return needsHighlighting
        ? getThemeColor(visuals.refNodeColor, theme)
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
      return getThemeColor(getNodeColorById(node.id as string), theme)
    }
    return highlightColors[getNodeColorById(node.id as string)][visuals.nodeHighlight](opacity)
  }

  const labelTextColor = useMemo(
    () => getThemeColor(visuals.labelTextColor, theme),
    [visuals.labelTextColor, emacsTheme],
  )

  const labelBackgroundColor = useMemo(
    () => getThemeColor(visuals.labelBackgroundColor, theme),
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

  const scaleRef = useRef(1)
  const graphCommonProps: ComponentPropsWithoutRef<typeof TForceGraph2D> = {
    graphData: scope.nodeIds.length ? scopedGraphData : filteredGraphData,
    width: windowWidth,
    height: windowHeight,
    backgroundColor: getThemeColor(visuals.backgroundColor, theme),
    warmupTicks: scope.nodeIds.length === 1 ? 100 : scope.nodeIds.length > 1 ? 20 : 0,
    onZoom: ({ k, x, y }) => (scaleRef.current = k),
    nodeColor: (node) => {
      return getNodeColor(node as OrgRoamNode, theme)
    },
    nodeRelSize: visuals.nodeRel,
    nodeVal: (node) => {
      return nodeSize(node) / Math.pow(scaleRef.current, visuals.nodeZoomSize)
    },
    nodeCanvasObject: (node, ctx, globalScale) => {
      drawLabels({
        nodeRel: visuals.nodeRel,
        filteredLinksByNodeId: filteredLinksByNodeIdRef.current,
        lastHoverNode: lastHoverNode.current,
        ...{
          node,
          ctx,
          globalScale,
          highlightedNodes,
          previouslyHighlightedNodes,
          visuals,
          opacity,
          nodeSize,
          labelTextColor,
          labelBackgroundColor,
          hoverNode,
        },
      })
    },
    nodeCanvasObjectMode: () => 'after',

    linkDirectionalParticles: visuals.particles ? visuals.particlesNumber : undefined,
    linkDirectionalArrowLength: visuals.arrows ? visuals.arrowsLength : undefined,
    linkDirectionalArrowRelPos: visuals.arrowsPos,
    linkDirectionalArrowColor: visuals.arrowsColor
      ? () => getThemeColor(visuals.arrowsColor, theme)
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

      return getLinkColor(sourceId as string, targetId as string, needsHighlighting, theme)
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
      //contextMenu.onClose()
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
    /* onBackgroundClick: () => {
     *   contextMenu.onClose()
     *   setHoverNode(null)
     *   if (scope.nodeIds.length === 0) {
     *     return
     *   }
     *   if (mouse.backgroundExitsLocal) {
     *     setScope((currentScope: Scope) => ({
     *       ...currentScope,
     *       nodeIds: [],
     *     }))
     *   }
     * }, */
    onNodeHover: (node) => {
      if (!visuals.highlight) {
        return
      }
      if (dragging) {
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
      //contextMenu.onClose()
      setHoverNode(node)
      setDragging(true)
    },
    onNodeDragEnd: () => {
      setHoverNode(null)
      setDragging(false)
    },
  }

  return (
    <Box overflow="hidden" onClick={contextMenu.onClose}>
      {threeDim ? (
        <ForceGraph3D
          ref={graphRef}
          {...graphCommonProps}
          nodeThreeObjectExtend={true}
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
            sprite.color = getThemeColor(visuals.labelTextColor, theme)
            sprite.backgroundColor = getThemeColor(visuals.labelBackgroundColor, theme)
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
}

function isLinkRelatedToNode(link: LinkObject, node: NodeObject | null) {
  return (
    (link.source as NodeObject)?.id! === node?.id! || (link.target as NodeObject)?.id! === node?.id!
  )
}

function numberWithinRange(num: number, min: number, max: number) {
  return Math.min(Math.max(num, min), max)
}

export function normalizeLinkEnds(link: OrgRoamLink | LinkObject): [string, string] {
  // we need to cover both because force-graph modifies the original data
  // but if we supply the original data on each render, the graph will re-render sporadically
  const sourceId =
    typeof link.source === 'object' ? (link.source.id! as string) : (link.source as string)
  const targetId =
    typeof link.target === 'object' ? (link.target.id! as string) : (link.target as string)
  return [sourceId, targetId]
}

export function getThemeColor(name: string, theme: any) {
  return name.split('.').reduce((o, i) => o[i], theme.colors)
}

export function hexToRGBA(hex: string, opacity: number) {
  return (
    'rgba(' +
    (hex = hex.replace('#', ''))
      .match(new RegExp('(.{' + hex.length / 3 + '})', 'g'))!
      .map((l) => parseInt(hex.length % 2 ? l + l : l, 16))
      .concat(isFinite(opacity) ? opacity : 1)
      .join(',') +
    ')'
  )
}
