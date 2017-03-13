import React, { Component } from 'react'

import {Tabs, Tab} from 'material-ui/Tabs'
import {List} from 'material-ui/List'

import AuthoredListItem from './AuthoredListItem';
import GetterHOC from '../hoc/GetterHOC'

const User = (props) => {
  const { claimItems, argumentItems } = props

  return (
    <Tabs>
      <Tab label="Claims" >
        <List>
          { claimItems.map(item =>
              <AuthoredListItem
                text={item.claimText}
                authorId={item.claimAuthorId}
                authorName={item.claimAuthorName}
                hrefPath={'/claims/' + item.claimId}
              />
            )
          }
        </List>
      </Tab>
      <Tab label="Arguments" >
        <List>
          { argumentItems.map(item =>
              <AuthoredListItem
                text={item.argumentText}
                authorId={item.argumentAuthorId}
                authorName={item.argumentAuthorName}
                hrefPath={'/arguments/' + item.argumentId}
              />
            )
          }
        </List>
      </Tab>
    </Tabs>
  )
}

export default GetterHOC(
  User,
  (props) => ([
    {
      path: props.location.pathName, // TODO
      mapResponseToProps: (resp) => {user: JSON.parse(resp.body)}
    },
    {
      path: props.location.pathName + '/items/arguments',
      mapResponseToProps: (resp) => {argumentItems: JSON.parse(resp.body)}
    },
    {
      path: props.location.pathName + '/items/claims',
      mapResponseToProps: (resp) => {claimItems: JSON.parse(resp.body)}
    }
  ])
)
