import React, { useEffect, useMemo, useState } from 'react'
import { OrgRoamNode } from '../api'
import { NodeByCite, NodeById } from '../pages/index'
import { ProcessedOrg } from './processOrg'

export interface UniOrgProps {
  nodeById: NodeById
  previewNode: any
  setPreviewNode: any
  nodeByCite: NodeByCite
  setSidebarHighlightedNode: any
}

export const UniOrg = (props: UniOrgProps) => {
  const { setSidebarHighlightedNode, nodeById, nodeByCite, previewNode, setPreviewNode } = props

  const [previewText, setPreviewText] = useState('')

  const file = encodeURIComponent(previewNode.file)
  useEffect(() => {
    fetch(`api/notes/${file}`)
      .then((res) => {
        return res.text()
      })
      .then((res) => {
        if (res !== 'error') {
          setPreviewText(res)
        }
      })
      .catch((e) => {
        console.log(e)
        return 'Could not fetch the text for some reason, sorry!\n\n This can happen because you have an id with forward slashes (/) in it.'
      })
  }, [previewNode.id])

  useEffect(() => {
    console.log('mount')
    return () => console.log('unmount')
  }, [])

  return (
    <>
      {previewNode?.id && (
        <ProcessedOrg
          {...{
            nodeById,
            previewNode,
            setPreviewNode,
            previewText,
            nodeByCite,
            setSidebarHighlightedNode,
          }}
        />
      )}
    </>
  )
}
