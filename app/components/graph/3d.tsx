
          ref={fgRef}
          //graphData={!physics.local ? gData : localGraphData }
          graphData={gData}
          nodeAutoColorBy={physics.colorful ? "id" : undefined}
          nodeColor={
            !physics.colorful
              ? (node) => {
                  if (highlightNodes.size === 0) {
                    return "rgb(100, 100, 100, 1)"
                  } else {
                    return highlightNodes.has(node) ? "purple" : "rgb(50, 50, 50, 0.5)"
                  }
                }
              : undefined
          }
          linkAutoColorBy={physics.colorful ? "target" : undefined}
          //linkAutoColorBy={(d) => gData.nodes[d.source].id % GROUPS}
          linkColor={
            !physics.colorful
              ? (link) => {
                  if (highlightLinks.size === 0) {
                    return "rgb(50, 50, 50, 0.8)"
                  } else {
                    return highlightLinks.has(link) ? "purple" : "rgb(50, 50, 50, 0.2)"
                  }
                }
              : undefined
          }
          linkDirectionalParticles={physics.particles}
          nodeLabel={(node) => node.title}
          linkWidth={(link) =>
            highlightLinks.has(link) ? 3 * physics.linkWidth : physics.linkWidth
          }
          linkOpacity={physics.linkOpacity}
          nodeRelSize={physics.nodeRel}
          nodeVal={(node) =>
            highlightNodes.has(node) ? node.neighbors.length + 5 : node.neighbors.length + 3
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
                    console.log("didnt crash here 2")
                    sprite.color = "#ffffff"
                    sprite.textHeight = 8
                    return sprite
                  } else {
                    return undefined
                  }
                }
          }
          nodeThreeObjectExtend={true}
          onNodeClick={selectClick}
