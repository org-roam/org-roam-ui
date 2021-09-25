import { Box, StackDivider, VStack } from '@chakra-ui/react'
import React from 'react'
import { ColorMenu } from './ColorMenu'
import { colorList, initialVisuals } from '../config'
import { EnableSection } from './EnableSection'
import { SliderWithInfo } from './SliderWithInfo'

export interface NodesNLinksPanelProps {
  visuals: typeof initialVisuals
  setVisuals: any
  threeDim: boolean
}

export const NodesNLinksPanel = (props: NodesNLinksPanelProps) => {
  const { visuals, setVisuals, threeDim } = props
  return (
    <VStack
      spacing={2}
      justifyContent="flex-start"
      divider={<StackDivider borderColor="gray.500" />}
      align="stretch"
      color="gray.800"
    >
      <Box>
        <SliderWithInfo
          label="Node size"
          value={visuals.nodeRel}
          onChange={(value) => setVisuals({ ...visuals, nodeRel: value })}
        />
        <SliderWithInfo
          label="Node connections size scale"
          value={visuals.nodeSizeLinks}
          min={0}
          max={2}
          onChange={(value) => setVisuals({ ...visuals, nodeSizeLinks: value })}
        />
        <SliderWithInfo
          label="Node zoom invariance"
          value={visuals.nodeZoomSize}
          min={0}
          max={2}
          infoText="How much the graph will try to keep the nodesize consistent across zoom scales. 0 is no consistency, node will always be their true size, 1 is linear, 2 is quadratic."
          onChange={(value) =>
            setVisuals((prev: typeof initialVisuals) => ({
              ...prev,
              nodeZoomSize: value,
            }))
          }
        />
        {threeDim && (
          <>
            <SliderWithInfo
              label="Node opacity"
              value={visuals.nodeOpacity}
              min={0}
              max={1}
              onChange={(value) => setVisuals({ ...visuals, nodeOpacity: value })}
            />
            <SliderWithInfo
              label="Node resolution"
              value={visuals.nodeResolution}
              min={5}
              max={32}
              step={1}
              onChange={(value) => setVisuals({ ...visuals, nodeResolution: value })}
            />
          </>
        )}
        <SliderWithInfo
          label="Link width"
          value={visuals.linkWidth}
          onChange={(value) => setVisuals({ ...visuals, linkWidth: value })}
        />
        {threeDim && (
          <SliderWithInfo
            label="Link opacity"
            min={0}
            max={1}
            value={visuals.linkOpacity}
            onChange={(value) => setVisuals({ ...visuals, linkOpacity: value })}
          />
        )}
        <EnableSection
          label="Link arrows"
          value={visuals.arrows}
          onChange={() => setVisuals({ ...visuals, arrows: !visuals.arrows })}
        >
          <SliderWithInfo
            label="Arrow size"
            value={visuals.arrowsLength / 10}
            onChange={(value) => setVisuals({ ...visuals, arrowsLength: 10 * value })}
          />
          <SliderWithInfo
            label="Arrow Position"
            value={visuals.arrowsPos}
            min={0}
            max={1}
            step={0.01}
            onChange={(value) => setVisuals({ ...visuals, arrowsPos: value })}
          />
          <ColorMenu
            colorList={colorList}
            label="Arrow Color"
            key="arrow"
            setVisuals={setVisuals}
            value="arrowsColor"
            visValue={visuals.arrowsColor}
          />
        </EnableSection>
        <EnableSection
          label="Directional Particles"
          value={visuals.particles}
          onChange={() => setVisuals({ ...visuals, particles: !visuals.particles })}
        >
          <SliderWithInfo
            label="Particle Number"
            value={visuals.particlesNumber}
            max={5}
            step={1}
            onChange={(value) => setVisuals({ ...visuals, particlesNumber: value })}
          />
          <SliderWithInfo
            label="Particle Size"
            value={visuals.particlesWidth}
            onChange={(value) => setVisuals({ ...visuals, particlesWidth: value })}
          />
        </EnableSection>
      </Box>
    </VStack>
  )
}
