export type OrgRoamGraphReponse = {
  nodes: OrgRoamNode[]
  links: OrgRoamLink[]
}

export type OrgRoamNode = {
  id: string
  file: string
  title: string
}

export type OrgRoamLink = {
  source: string
  dest: string
}
