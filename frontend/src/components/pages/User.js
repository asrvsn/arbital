import React, { Component } from 'react'

import {Tabs, Tab} from 'material-ui/Tabs'
import {List, ListItem} from 'material-ui/List'
import {Card, CardActions, CardHeader, CardMedia, CardTitle, CardText} from 'material-ui/Card';
import Divider from 'material-ui/Divider';

import ArgumentListItem from '../items/ArgumentListItem';
import ClaimListItem from '../items/ClaimListItem';
import GetterHOC from '../hoc/GetterHOC'

const User = (props) => {
  const { page, router } = props
  const { user, claims, args } = page

  return (
    <Card>
      <CardTitle
        title={user.name}
        subtitle={
          <div>
            <div>{user.email}</div>
            <div>{`Registered on ${user.registrationDate}`}</div>
          </div>
        }
      />

      <Divider />

      <Tabs>
        <Tab label={`Claims (${claims.length})`}>
          <List>
            { (claims.length > 0) ?
                claims.map(claim =>
                  <ClaimListItem
                    key={claim.id}
                    claim={claim}
                    router={router}
                  />
                )
              :
                <ListItem primaryText="(nothing)" />
            }
          </List>
        </Tab>
        <Tab label={`Arguments (${args.length})`}>
          <List>
            { args.map(arg =>
                <ArgumentListItem
                  key={arg.id}
                  argument={arg}
                  router={router}
                />
              )
            }
          </List>
        </Tab>
      </Tabs>
    </Card>
  )
}

export default GetterHOC(
  User,
  (props) => ({
    USER: {
      path: props.location.pathname + '/page',
      mapResponseToProps: (resp) => ({page: resp})
    },
  })
)
