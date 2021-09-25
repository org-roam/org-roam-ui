import { Box, Select, StackDivider, VStack } from '@chakra-ui/react'
import React from 'react'
import { initialVisuals } from '../config'
import { EnableSection } from './EnableSection'
import { SliderWithInfo } from './SliderWithInfo'

export interface HighlightingPanelProps {
  visuals: typeof initialVisuals
  setVisuals: any
}
export const HighlightingPanel = (props: HighlightingPanelProps) => {
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
          label="Highlight"
          onChange={() =>
            setVisuals((visuals: typeof initialVisuals) => ({
              ...visuals,
              highlight: !visuals.highlight,
            }))
          }
          value={visuals.highlight}
        >
          <VStack
            spacing={1}
            justifyContent="flex-start"
            divider={<StackDivider borderColor="gray.400" />}
            align="stretch"
            paddingLeft={0}
          >
            <SliderWithInfo
              label="Highlight Link Thickness"
              value={visuals.highlightLinkSize}
              onChange={(value) =>
                setVisuals((visuals: typeof initialVisuals) => ({
                  ...visuals,
                  highlightLinkSize: value,
                }))
              }
            />
            <SliderWithInfo
              label="Highlight Node Size"
              value={visuals.highlightNodeSize}
              onChange={(value) =>
                setVisuals((visuals: typeof initialVisuals) => ({
                  ...visuals,
                  highlightNodeSize: value,
                }))
              }
            />
            <SliderWithInfo
              min={0}
              max={1}
              label="Highlight Fade"
              value={visuals.highlightFade}
              onChange={(value) =>
                setVisuals((visuals: typeof initialVisuals) => ({
                  ...visuals,
                  highlightFade: value,
                }))
              }
            />
            <EnableSection
              label="Highlight Animation"
              onChange={() => {
                setVisuals((visuals: typeof initialVisuals) => ({
                  ...visuals,
                  highlightAnim: !visuals.highlightAnim,
                }))
              }}
              value={visuals.highlightAnim}
            >
              <SliderWithInfo
                label="Animation speed"
                onChange={(v) =>
                  setVisuals((visuals: typeof initialVisuals) => ({
                    ...visuals,
                    animationSpeed: v,
                  }))
                }
                value={visuals.animationSpeed}
                infoText="Slower speed has a chance of being buggy"
                min={50}
                max={1000}
                step={10}
              />
              <Select
                placeholder={visuals.algorithmName}
                onChange={(v) => {
                  setVisuals((visuals: typeof initialVisuals) => ({
                    ...visuals,
                    algorithmName: v.target.value,
                  }))
                }}
              >
                {visuals.algorithmOptions.map((opt: string) => (
                  <option key={opt} value={opt}>
                    {opt}
                  </option>
                ))}
              </Select>
            </EnableSection>
          </VStack>
        </EnableSection>
      </Box>
    </VStack>
  )
}
