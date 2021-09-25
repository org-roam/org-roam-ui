import { CUIAutoComplete } from 'chakra-ui-autocomplete'
import React, { useState } from 'react'
import { initialFilter } from '../config'

export interface TagPanelProps {
  tags: string[]
  filter: typeof initialFilter
  setFilter: any
  highlightColor: string
  mode: string
}

export const TagPanel = (props: TagPanelProps) => {
  const { filter, setFilter, tags, highlightColor, mode } = props
  const tagArray = tags.map((tag) => {
    return { value: tag, label: tag }
  })

  const currentTags = mode === 'blacklist' ? 'tagsBlacklist' : 'tagsWhitelist'
  const [selectedItems, setSelectedItems] = useState<typeof tagArray>(
    filter[currentTags].map((tag) => {
      return { value: tag, label: tag }
    }),
  )

  return (
    <CUIAutoComplete
      items={tagArray}
      label={'Add tag to ' + mode}
      placeholder=" "
      onCreateItem={(item) => null}
      disableCreateItem={true}
      selectedItems={selectedItems}
      onSelectedItemsChange={(changes) => {
        if (changes.selectedItems) {
          setSelectedItems(changes.selectedItems)
          setFilter({ ...filter, [currentTags]: changes.selectedItems.map((item) => item.value) })
        }
      }}
      listItemStyleProps={{ overflow: 'hidden' }}
      highlightItemBg="gray.400"
      toggleButtonStyleProps={{ variant: 'outline' }}
      inputStyleProps={{
        focusBorderColor: highlightColor,
        color: 'gray.800',
        borderColor: 'gray.600',
      }}
      tagStyleProps={{
        rounded: 'full',
        bg: highlightColor,
        height: 8,
        paddingLeft: 4,
        fontWeight: 'bold',
      }}
      hideToggleButton
      itemRenderer={(selected) => selected.label}
    />
  )
}
