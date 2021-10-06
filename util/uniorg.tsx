import React, { useEffect, useState } from 'react'
import { NodeById } from '../pages/index'
import { ProcessedOrg } from './processOrg'

export interface UniOrgProps {
  nodeById: NodeById
  previewNode: any
  setPreviewNode: any
  getText: any
}

export const UniOrg = (props: UniOrgProps) => {
  const { nodeById, previewNode, setPreviewNode, getText } = props

  const [previewText, setPreviewText] = useState('')

  useEffect(() => {
    if (previewNode?.id) {
      getText(previewNode?.id, setPreviewText)
    }
  }, [previewNode?.id])

  return (
    <ProcessedOrg
      {...{
        getText,
        nodeById,
        previewNode,
        setPreviewNode,
        previewText,
      }}
    />
  )
}
