import { Box, StackDivider, VStack } from '@chakra-ui/react'
import React from 'react'
import { ColorMenu } from './ColorMenu'
import { colorList, initialVisuals } from '../config'
import { EnableSection } from './EnableSection'
import { SliderWithInfo } from './SliderWithInfo'

export interface CitationsPanelProps {
  visuals: typeof initialVisuals
  setVisuals: any
}
export const CitationsPanel = (props: CitationsPanelProps) => {
  const { visuals, setVisuals } = props
  return (
    <VStack
      spacing={2}
      justifyContent="flex-start"
      divider={<StackDivider borderColor="gray.500" />}
      align="stretch"
      color="gray.800"
    >
      <Box>
        <EnableSection
          label="Dash cite links"
          infoText="Add dashes to citation links made with org-roam-bibtex"
          value={visuals.citeDashes}
          onChange={() => setVisuals({ ...visuals, citeDashes: !visuals.citeDashes })}
        >
          <SliderWithInfo
            label="Dash length"
            value={visuals.citeDashLength / 10}
            onChange={(value) => setVisuals({ ...visuals, citeDashLength: value * 10 })}
          />
          <SliderWithInfo
            label="Gap length"
            value={visuals.citeGapLength / 5}
            onChange={(value) => setVisuals({ ...visuals, citeGapLength: value * 5 })}
          />
        </EnableSection>
        <ColorMenu
          colorList={colorList}
          label="Citation node color"
          setVisuals={setVisuals}
          value={'citeNodeColor'}
          visValue={visuals.citeNodeColor}
        />
        <ColorMenu
          colorList={colorList}
          label="Citation link color"
          setVisuals={setVisuals}
          value={'citeLinkColor'}
          visValue={visuals.citeLinkColor}
        />
        <ColorMenu
          colorList={colorList}
          label="Reference link highlight"
          setVisuals={setVisuals}
          value={'citeLinkHighlightColor'}
          visValue={visuals.citeLinkHighlightColor}
        />
        <EnableSection
          label="Dash ref links"
          infoText="Add dashes to citation links, whose target has a note, made with org-roam-bibtex"
          value={visuals.refDashes}
          onChange={() => setVisuals({ ...visuals, refDashes: !visuals.refDashes })}
        >
          <SliderWithInfo
            label="Dash length"
            value={visuals.refDashLength / 10}
            onChange={(value) => setVisuals({ ...visuals, refDashLength: value * 10 })}
          />
          <SliderWithInfo
            label="Gap length"
            value={visuals.refGapLength / 5}
            onChange={(value) => setVisuals({ ...visuals, refGapLength: value * 5 })}
          />
        </EnableSection>
        <ColorMenu
          colorList={colorList}
          label="Reference node color"
          setVisuals={setVisuals}
          value={'refNodeColor'}
          visValue={visuals.refNodeColor}
        />
        <ColorMenu
          colorList={colorList}
          label="Reference link color"
          setVisuals={setVisuals}
          value={'refLinkColor'}
          visValue={visuals.refLinkColor}
        />
        <ColorMenu
          colorList={colorList}
          label="Reference link highlight"
          setVisuals={setVisuals}
          value={'refLinkHighlightColor'}
          visValue={visuals.refLinkHighlightColor}
        />
      </Box>
    </VStack>
  )
}
