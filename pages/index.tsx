import React, { useEffect, useRef, useState } from 'react'
import dynamic from 'next/dynamic'
import { usePersistantState } from '../util/persistant-state'
const d3promise = import('d3-force-3d')

import type { ForceGraph2D as TForceGraph2D } from 'react-force-graph'
import { OrgRoamGraphReponse, OrgRoamLink, OrgRoamNode } from '../api'
import { GraphData, NodeObject } from 'force-graph'

import { useWindowSize } from '@react-hook/window-size'

// react-force-graph fails on import when server-rendered
// https://github.com/vasturiano/react-force-graph/issues/155
const ForceGraph2D = (
  !!global.window ? require('react-force-graph').ForceGraph2D : null
) as typeof TForceGraph2D

export type NodeById = { [nodeId: string]: OrgRoamNode | undefined }
export type LinksByNodeId = { [nodeId: string]: OrgRoamLink[] | undefined }

const initialPhysics = {
  charge: -350,
  collision: true,
  linkStrength: 0.1,
  linkIts: 1,
  particles: 0,
  linkOpacity: 0.4,
  linkWidth: 1,
  particleWidth: 4,
  nodeRel: 4,
  labels: true,
  labelScale: 1.5,
  alphaDecay: 0.16,
  alphaTarget: 0,
  velocityDecay: 0.25,
  gravity: 0.5,
  gravityOn: true,
  hover: true,
  colorful: true,
  galaxy: true,
  rootId: 0,
}

const initialTheme = {
  base1: '#1c1f24',
  base2: '#21272d',
  base3: '#23272e',
  base4: '#484854',
  base5: '#62686E',
  base6: '#757B80',
  base7: '#9ca0a4',
  base8: '#DFDFDF',
  bg: '#242730',
  'bg-alt': '#2a2e38',
  blue: '#51afef',
  cyan: '#5cEfFF',
  'dark-blue': '#1f5582',
  'dark-cyan': '#6A8FBF',
  fg: '#bbc2cf',
  'fg-alt': '#5D656B',
  green: '#7bc275',
  grey: '#484854',
  magenta: '#C57BDB',
  orange: '#e69055',
  red: '#ff665c',
  teal: '#4db5bd',
  violet: '#a991f1',
  yellow: '#FCCE7B',
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
  const [theme, setTheme] = useState(initialTheme)
  const [graphData, setGraphData] = useState<GraphData | null>(null)

  const nodeByIdRef = useRef<NodeById>({})
  const linksByNodeIdRef = useRef<LinksByNodeId>({})

  useEffect(() => {
    fetch('http://localhost:35901/theme')
      .then((res) => res.json())
      .then((emacsTheme) => {
        setTheme(emacsTheme)
      })
    fetch('http://localhost:35901/graph')
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
  }, [])

  const [threeDim, setThreeDim] = useState(false)
  const [local, setLocal] = useState(false)

  if (!graphData) {
    return null
  }

  return (
    <div>
      <Graph
        nodeById={nodeByIdRef.current!}
        linksByNodeId={linksByNodeIdRef.current!}
        {...{
          physics,
          graphData,
          threeDim,
          local,
        }}
      />
    </div>
  )
}

export interface GraphProps {
  nodeById: NodeById
  linksByNodeId: LinksByNodeId
  graphData: GraphData
  physics: typeof initialPhysics
  threeDim: boolean
  local: boolean
}

export const Graph = function (props: GraphProps) {
  const { physics, graphData, threeDim, local, linksByNodeId, nodeById } = props

  const forceGraphRef = useRef<any>(null)

  // react-force-graph does not track window size
  // https://github.com/vasturiano/react-force-graph/issues/233
  // does not work below a certain width
  const [windowWidth, windowHeight] = useWindowSize()

  const [hoverNode, setHoverNode] = useState<NodeObject | null>(null)
  const [selectedNode, setSelectedNode] = useState<NodeObject | null>()

  const centralHighlightedNode = selectedNode ?? hoverNode
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

  useEffect(() => {
    ;(async () => {
      const fg = forceGraphRef.current
      const d3 = await d3promise
      //fg.d3Force('center').strength(0.05);
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
      fg.d3Force('link').strength(physics.linkStrength)
      fg.d3Force('link').iterations(physics.linkIts)
      physics.collision
        ? fg.d3Force('collide', d3.forceCollide().radius(20))
        : fg.d3Force('collide', null)
      fg.d3Force('charge').strength(physics.charge)
    })()
  })

  // Normally the graph doesn't update when you just change the physics parameters
  // This forces the graph to make a small update when you do
  useEffect(() => {
    forceGraphRef.current?.d3ReheatSimulation()
  }, [physics])

  //shitty handler to check for doubleClicks
  const [doubleClick, setDoubleClick] = useState(0)
  const [localGraphData, setLocalGraphData] = useState<any>({
    nodes: [],
    links: [],
  })

  const selectClick = (node: NodeObject, event: any) => {
    window.open('org-protocol://roam-node?node=' + node.id, '_self')

    if (event.timeStamp - doubleClick < 400) {
      // getLocalGraphData(node)
    }
    // setDoubleClick(event.timeStamp)
    if (node) {
      return setSelectedNode(node)
    }
  }

  return (
    <div style={{ position: 'absolute' }}>
      <ForceGraph2D
        ref={forceGraphRef}
        graphData={local ? localGraphData : graphData}
        width={windowWidth}
        height={windowHeight}
        nodeColor={(node) => {
          if (!physics.colorful) {
            if (Object.keys(highlightedNodes).length === 0) {
              return 'rgb(100, 100, 100, 1)'
            }
            return highlightedNodes[node.id!] ? '#a991f1' : 'rgb(50, 50, 50, 0.5)'
          }

          const palette = [
            '#ff665c',
            '#e69055',
            '#7bc275',
            '#4db5bd',
            '#FCCE7B',
            '#51afef',
            '#1f5582',
            '#C57BDB',
            '#a991f1',
            '#5cEfFF',
            '#6A8FBF',
          ]

          // random
          return palette[0]
          if (node.neighbors.length === 1 || node.neighbors.length === 2) {
            return palette[node.neighbors[0].index % 11]
          }

          return palette[node.index % 11]
        }}
        linkColor={(link) => {
          if (Object.keys(highlightedNodes).length === 0) {
            return 'rgb(50, 50, 50, 0.8)'
          }

          const linkIsHighlighted =
            (link.source as NodeObject).id! === centralHighlightedNode?.id! ||
            (link.target as NodeObject).id! === centralHighlightedNode?.id!

          return linkIsHighlighted ? '#a991f1' : 'rgb(50, 50, 50, 0.2)'
        }}
        linkDirectionalParticles={physics.particles}
        linkDirectionalParticleWidth={physics.particleWidth}
        nodeLabel={(node) => (node as OrgRoamNode).title}
        linkWidth={(link) => {
          const linkIsHighlighted =
            (link.source as NodeObject).id! === centralHighlightedNode?.id! ||
            (link.target as NodeObject).id! === centralHighlightedNode?.id!

          return linkIsHighlighted ? 3 * physics.linkWidth : physics.linkWidth
        }}
        nodeRelSize={physics.nodeRel}
        nodeVal={(node) => {
          const links = props.linksByNodeId[node.id!] ?? []
          const basicSize = 3 + links.length
          const highlightSize = highlightedNodes[node.id!] ? 2 : 0
          return basicSize + highlightSize
        }}
        nodeCanvasObject={(node, ctx, globalScale) => {
          if (!physics.labels) {
            return
          }

          if (globalScale <= physics.labelScale && !highlightedNodes[node.id!]) {
            return
          }

          const nodeTitle = (node as OrgRoamNode).title
          const label = nodeTitle.substring(0, Math.min(nodeTitle.length, 30))
          // const label = 'label'
          const fontSize = 12 / globalScale
          const textWidth = ctx.measureText(label).width
          const bckgDimensions = [textWidth * 1.1, fontSize].map((n) => n + fontSize * 0.5) as [
            number,
            number,
          ] // some padding

          const fadeFactor = Math.min(
            (3 * (globalScale - physics.labelScale)) / physics.labelScale,
            1,
          )

          // draw label background
          ctx.fillStyle =
            'rgba(20, 20, 20, ' +
            (highlightedNodes.length === 0
              ? 0.5 * fadeFactor
              : highlightedNodes[node.id!]
              ? 0.5
              : 0.15 * fadeFactor) +
            ')'
          ctx.fillRect(
            node.x! - bckgDimensions[0] / 2,
            node.y! - bckgDimensions[1] / 2,
            ...bckgDimensions,
          )

          // draw label text
          ctx.textAlign = 'center'
          ctx.textBaseline = 'middle'
          ctx.fillStyle =
            'rgb(255, 255, 255, ' +
            (highlightedNodes.length === 0
              ? fadeFactor
              : highlightedNodes[node.id!]
              ? 1
              : 0.3 * fadeFactor) +
            ')'
          ctx.font = `${fontSize}px Sans-Serif`
          ctx.fillText(label, node.x!, node.y!)
        }}
        nodeCanvasObjectMode={() => 'after'}
        d3AlphaDecay={physics.alphaDecay}
        d3AlphaMin={physics.alphaTarget}
        d3VelocityDecay={physics.velocityDecay}
        backgroundColor={'#242730'}
        onNodeClick={selectClick}
        onBackgroundClick={() => {
          setSelectedNode(null)
        }}
        onNodeHover={(node) => {
          if (!physics.hover) {
            return
          }
          setHoverNode(node)
        }}
      />
    </div>
  )
}
