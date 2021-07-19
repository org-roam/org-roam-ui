import * as React from 'react'
import { useState, useEffect, useRef, useMemo, useCallback } from 'react'
import { StyleProp, TextStyle, View, ViewStyle } from 'react-native'
import { observer } from 'mobx-react-lite'
import { color, typography } from '../../theme'
import { Text } from '../'
import { flatten } from 'ramda'

//import data from "../../data/miserables.json"
//import genRandomTree from "../../data/randomdata";
//import gData from "../../data/rando.json"

import {
  ForceGraph2D,
  ForceGraph3D,
  ForceGraphVR,
  ForceGraphAR,
} from 'react-force-graph'
import * as d3 from 'd3-force-3d'
//import * as three from "three"
import SpriteText from 'three-spritetext'

const CONTAINER: ViewStyle = {
  justifyContent: 'center',
}

const TEXT: TextStyle = {
  fontFamily: typography.primary,
  fontSize: 14,
  color: color.primary,
}

export interface GraphProps {
  /**
   * An optional style override useful for padding & margin.
   */
  style?: StyleProp<ViewStyle>
  physics
  gData
  setPhysics
  nodeIds: string[]
  threeDim
  setThreeDim
  local
  setLocal
}

/**
 * Describe your component here
 */
export const Graph = observer(function Graph(props: GraphProps): JSX.Element {
  const {
    style,
    physics,
    setPhysics,
    gData,
    threeDim,
    setThreeDim,
    local,
    setLocal,
  } = props
  const styles = flatten([CONTAINER, style])

  const fgRef = useRef()

  const GROUPS: number = 12
  const NODE_R: number = 8
  //const gData = genRandomTree(200);

  //const [charge, setCharge] = useState(-30);
  //const [link, setLink] = useState(-30);

  useEffect(() => {
    const fg = fgRef.current
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
  })

  // For the expandable version of the graph

  /* const nodesById = useMemo(() => {
     *   const nodesById = Object.fromEntries(gData.nodes.map((node) => [node.index, node]))
     *   console.log(nodesById)
     *   // link parent/children
     *   gData.nodes.forEach((node) => {
     *     typeof physics.rootId === "number"
     *       ? (node.collapsed = node.index !== physics.rootId)
     *       : (node.collapsed = node.id !== physics.rootId)
     *     node.childLinks = []
     *   })
     *   gData.links.forEach((link) => nodesById[link.sourceIndex].childLinks.push(link))
     *   return nodesById
     * }, [gData])
     * const getPrunedTree = useCallback(() => {
     *   const visibleNodes = []
     *   const visibleLinks = []
     *   ;(function traverseTree(node = nodesById[physics.rootId]) {
     *     visibleNodes.push(node)
     *     if (node.collapsed) return
     *     visibleLinks.push(...node.childLinks)
     *     node.childLinks
     *       .map((link) =>
     *         typeof link.targetIndex === "object" ? link.targetIndex : nodesById[link.targetIndex],
     *       ) // get child node
     *       .forEach(traverseTree)
     *   })()

     *   return { nodes: visibleNodes, links: visibleLinks }
     * }, [nodesById])
     * const [prunedTree, setPrunedTree] = useState(getPrunedTree())
     */
  const handleNodeClick = useCallback((node) => {
    node.collapsed = !node.collapsed // toggle collapse state
    setPrunedTree(getPrunedTree())
  }, [])

  //highlighting
  const [highlightNodes, setHighlightNodes] = useState(new Set())
  const [highlightLinks, setHighlightLinks] = useState(new Set())
  const [hoverNode, setHoverNode] = useState(null)

  const updateHighlight = () => {
    setHighlightNodes(highlightNodes)
    setHighlightLinks(highlightLinks)
  }

  const handleBackgroundClick = (event) => {
    highlightNodes.clear()
    highlightLinks.clear()

    setSelectedNode(null)
    updateHighlight()
  }

  const handleNodeHover = (node) => {
    console.log('hover')
    if (!selectedNode) {
      highlightNodes.clear()
      highlightLinks.clear()
      if (node) {
        highlightNodes.add(node)
        node.neighbors.forEach((neighbor) => highlightNodes.add(neighbor))
        node.links.forEach((link) => highlightLinks.add(link))
      }

      setHoverNode(node || null)
      updateHighlight()
    }
  }

  const handleLinkHover = (link) => {
    highlightNodes.clear()
    highlightLinks.clear()

    if (link) {
      highlightLinks.add(link)
      highlightNodes.add(link.source)
      highlightNodes.add(link.target)
    }

    updateHighlight()
  }

  // Normally the graph doesn't update when you just change the physics parameters
  // This forces the graph to make a small update when you do
  useEffect(() => {
    fgRef.current.d3ReheatSimulation()
  }, [physics])
  /* const paintRing = useCallback((node, ctx) => {
   *   // add ring just for highlighted nodes
   *   ctx.beginPath();
   *   ctx.arc(node.x, node.y, NODE_R * 1.4, 0, 2 * Math.PI, false);
   *   ctx.fillStyle = node === hoverNode ? 'red' : 'orange';
   *   ctx.fill();
   * }, [hoverNode]);
   */

  /* autoPauseRedraw={false}
linkWidth={link => highlightLinks.has(link) ? 5 : 1}
linkDirectionalParticles={4}
linkDirectionalParticleWidth={link => highlightLinks.has(link) ? 4 : 0}
nodeCanvasObjectMode={node => highlightNodes.has(node) ? 'before' : undefined}
nodeCanvasObject={paintRing}
onNodeHover={handleNodeHover}
onLinkHover={handleLinkHover}
        nodeRelSize={NODE_R} */

  //nodeColor={(node) =>
  //  !node.childLinks.length ? "green" : node.collapsed ? "red" : "yellow"
  //}

  const [selectedNode, setSelectedNode] = useState({})

  //shitty handler to check for doubleClicks
  const [doubleClick, setDoubleClick] = useState(0)
  const [localGraphData, setLocalGraphData] = useState({ nodes: [], links: [] })

  useEffect(() => {
    localGraphData.nodes.length && !local && setLocal(true)
  }, [localGraphData])

  const getLocalGraphData = (node) => {
    console.log(localGraphData)
    localGraphData.nodes.length
      ? setLocalGraphData({ nodes: [], links: [] })
      : null
    let g = localGraphData
    console.log(g.nodes)
    if (!node.local) {
      g = { nodes: [], links: [] }
      console.log('length is 0')
      node.local = true //keep track of these boys
      g.nodes.push(node) //only add the clicked node if its the first
    }
    node.links.length &&
      node.links.forEach((neighborLink) => {
        if (!neighborLink.local) {
          console.log('0')
          neighborLink.local = true
          g.links.push(neighborLink)
          console.log(neighborLink)
          const targetNode = gData.nodes[neighborLink.targetIndex]
          const sourceNode = gData.nodes[neighborLink.sourceIndex]
          if (targetNode.id !== sourceNode.id) {
            if (targetNode.id === node.id) {
              console.log('1. I am the target, the source is ')
              console.log(sourceNode)
              if (!sourceNode.local) {
                console.log('2. The source is not local')
                sourceNode.local = true
                g.nodes.push(sourceNode)
              } else {
                console.log('2.5 The source is already local')
              }
            } else {
              console.log('3. I am the source')
              if (!targetNode.local) {
                console.log('4. The target is not local.')
                targetNode.local = true
                g.nodes.push(targetNode)
              } else {
                console.log('The target is already local')
              }
            }
          }
        }
      })
    setLocalGraphData(g)
  }

  const selectClick = (node, event) => {
    window.open('org-protocol://roam-node?node=' + node.id, '_self')
    highlightNodes.clear()
    highlightLinks.clear()
    console.log(localGraphData)
    if (event.timeStamp - doubleClick < 400) {
      getLocalGraphData(node)
    }
    if (node) {
      highlightNodes.add(node)
      node.neighbors.forEach((neighbor) => highlightNodes.add(neighbor))
      node.links.forEach((link) => highlightLinks.add(link))
    }

    setSelectedNode(node || null)
    updateHighlight()
    setDoubleClick(event.timeStamp)
  }

  useEffect(() => {
    if (local && selectedNode) {
      getLocalGraphData(selectedNode)
    }
  }, [local])
  return (
    <View style={style}>
      {!threeDim ? (
        <ForceGraph2D
          ref={fgRef}
          //autoPauseRedraw={false}
          //graphData={gData}
          graphData={local ? localGraphData : gData}
          //nodeAutoColorBy={physics.colorful ? (node)=>node.index%GROUPS : undefined}
          nodeColor={
            !physics.colorful
              ? (node) => {
                  if (highlightNodes.size === 0) {
                    return 'rgb(100, 100, 100, 1)'
                  } else {
                    return highlightNodes.has(node)
                      ? '#a991f1'
                      : 'rgb(50, 50, 50, 0.5)'
                  }
                }
              : (node) => {
                  if (
                    node.neighbors.length === 1 ||
                    node.neighbors.length === 2
                  ) {
                    return [
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
                    ][node.neighbors[0].index % 11]
                  } else {
                    return [
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
                    ][node.index % 11]
                  }
                }
          }
          //linkAutoColorBy={physics.colorful ? ((d) => gData.nodes[d.sourceIndex].id % GROUPS) : undefined}
          linkColor={
            !physics.colorful
              ? (link) => {
                  if (highlightLinks.size === 0) {
                    return 'rgb(50, 50, 50, 0.8)'
                  } else {
                    return highlightLinks.has(link)
                      ? '#a991f1'
                      : 'rgb(50, 50, 50, 0.2)'
                  }
                }
              : (link) =>
                  [
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
                  ][gData.nodes[link.sourceIndex].index % 11]
          }
          linkDirectionalParticles={physics.particles}
          onNodeClick={selectClick}
          nodeLabel={(node) => node.title}
          linkWidth={(link) =>
            highlightLinks.has(link) ? 3 * physics.linkWidth : physics.linkWidth
          }
          linkOpacity={physics.linkOpacity}
          nodeRelSize={physics.nodeRel}
          nodeVal={(node) => {
            return highlightNodes.has(node)
              ? node.neighbors.length + 5
              : node.neighbors.length + 3
          }}
          linkDirectionalParticleWidth={physics.particleWidth}
          nodeCanvasObject={(node, ctx, globalScale) => {
            if (physics.labels) {
              if (
                globalScale > physics.labelScale ||
                highlightNodes.has(node)
              ) {
                const label = node.title.substring(
                  0,
                  Math.min(node.title.length, 30),
                )
                const fontSize = 12 / globalScale
                ctx.font = `${fontSize}px Sans-Serif`
                const textWidth = ctx.measureText(label).width
                const bckgDimensions = [textWidth * 1.1, fontSize].map(
                  (n) => n + fontSize * 0.5,
                ) // some padding
                const fadeFactor = Math.min(
                  (3 * (globalScale - physics.labelScale)) / physics.labelScale,
                  1,
                )

                ctx.fillStyle =
                  'rgba(20, 20, 20, ' +
                  (highlightNodes.size === 0
                    ? 0.5 * fadeFactor
                    : highlightNodes.has(node)
                    ? 0.5
                    : 0.15 * fadeFactor) +
                  ')'
                ctx.fillRect(
                  node.x - bckgDimensions[0] / 2,
                  node.y - bckgDimensions[1] / 2,
                  ...bckgDimensions,
                )

                ctx.textAlign = 'center'
                ctx.textBaseline = 'middle'
                ctx.fillStyle =
                  'rgb(255, 255, 255, ' +
                  (highlightNodes.size === 0
                    ? fadeFactor
                    : highlightNodes.has(node)
                    ? 1
                    : 0.3 * fadeFactor) +
                  ')'
                ctx.fillText(label, node.x, node.y)

                node.__bckgDimensions = bckgDimensions // to re-use in nodePointerAreaPaint
              }
            }
          }}
          nodeCanvasObjectMode={() => 'after'}
          onNodeHover={physics.hover ? handleNodeHover : null}
          //onLinkHover={physics.hover ? handleLinkHover : null}
          d3AlphaDecay={physics.alphaDecay}
          d3AlphaMin={physics.alphaTarget}
          d3VelocityDecay={physics.velocityDecay}
          onBackgroundClick={handleBackgroundClick}
          backgroundColor={'#242730'}
        />
      ) : (
        <ForceGraph3D
          ref={fgRef}
          graphData={!local ? gData : localGraphData}
          //graphData={gData}
          nodeColor={
            !physics.colorful
              ? (node) => {
                  if (highlightNodes.size === 0) {
                    return 'rgb(100, 100, 100, 1)'
                  } else {
                    return highlightNodes.has(node)
                      ? 'purple'
                      : 'rgb(50, 50, 50, 0.5)'
                  }
                }
              : (node) => {
                  if (
                    node.neighbors.length === 1 ||
                    node.neighbors.length === 2
                  ) {
                    return [
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
                    ][node.neighbors[0].index % 11]
                  } else {
                    return [
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
                    ][node.index % 11]
                  }
                }
          }
          //linkAutoColorBy={physics.colorful ? ((d) => gData.nodes[d.sourceIndex].id % GROUPS) : undefined}
          linkColor={
            !physics.colorful
              ? (link) => {
                  if (highlightLinks.size === 0) {
                    return 'rgb(50, 50, 50, 0.8)'
                  } else {
                    return highlightLinks.has(link)
                      ? 'purple'
                      : 'rgb(50, 50, 50, 0.2)'
                  }
                }
              : (link) =>
                  [
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
                  ][gData.nodes[link.sourceIndex].index % 11]
          }
          linkDirectionalParticles={physics.particles}
          nodeLabel={(node) => node.title}
          linkWidth={(link) =>
            highlightLinks.has(link) ? 3 * physics.linkWidth : physics.linkWidth
          }
          linkOpacity={physics.linkOpacity}
          nodeRelSize={physics.nodeRel}
          nodeVal={(node) =>
            highlightNodes.has(node)
              ? node.neighbors.length * 3
              : node.neighbors.length * 2
          }
          linkDirectionalParticleWidth={physics.particleWidth}
          onNodeHover={physics.hover ? handleNodeHover : null}
          d3AlphaDecay={physics.alphaDecay}
          d3AlphaMin={physics.alphaTarget}
          d3VelocityDecay={physics.velocityDecay}
          nodeThreeObject={
            !physics.labels
              ? undefined
              : (node) => {
                  if (highlightNodes.has(node)) {
                    console.log(node.title)
                    const sprite = new SpriteText(node.title.substring(0, 30))
                    console.log('didnt crash here 2')
                    sprite.color = '#ffffff'
                    sprite.textHeight = 8
                    return sprite
                  } else {
                    return undefined
                  }
                }
          }
          nodeThreeObjectExtend={true}
          onNodeClick={selectClick}
          onBackgroundClick={handleBackgroundClick}
          backgroundColor={'#242730'}
        />
      )}
    </View>
  )
})
