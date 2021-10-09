import React, { useEffect, useState } from 'react'
import Image from 'next/image'
import path from 'path'
//import '../../../public/placeholder.png'

export interface OrgImageProps {
  src: string
  file: string
}

export const OrgImage = (props: OrgImageProps) => {
  const { src, file } = props

  const [image, setImage] = useState<any>(null)

  /* )
*   .then((res) => res.blob())
*   .then((res) => setImage(res))
*   .catch((e) => {
*     setImage(null)
*     console.error(e)
*   })
}, [fullPath]) */

  const dir = path.dirname(file)
  const fullPath = encodeURIComponent(encodeURIComponent(path.join(dir, src)))

  const dumbLoader = ({ src, width, quality }: { [key: string]: string | number }) => {
    return `http://localhost:35901/img/${src}`
  }

  return (
    <Image layout="responsive" loader={dumbLoader} src={fullPath} alt="" width="70%" height="70%" />
  )
}
