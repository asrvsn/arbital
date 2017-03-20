import React, { Component, PropTypes } from 'react'

import {Card, CardActions, CardHeader, CardText, CardTitle} from 'material-ui/Card';
import FlatButton from 'material-ui/FlatButton';
import Divider from 'material-ui/Divider';
import {List, ListItem} from 'material-ui/List';

import AuthoredListItem from '../items/AuthoredListItem';
import GetterHOC from '../hoc/GetterHOC';

const styles = {
  chip: {
    margin: 4,
  },
}

const Argument = (props) => {
  const { page, session, router } = props
  const { arg, claims } = page

  const isMyArgument = session.user.id === arg.authorId

  return (
    <Card>
      <CardTitle
        title={arg.text}
        subtitle={arg.authorName}
      />
      { isMyArgument &&
        <CardActions>
          <FlatButton label="Update" />
          <FlatButton label="Delete" />
        </CardActions>
      }
      <Divider />
      <CardHeader subtitle="Supporting claims" />
      <CardText>
        <List>
          { claims.map(claim =>
              <AuthoredListItem
                key={claim.id}
                text={claim.text}
                authorId={claim.authorId}
                authorName={claim.authorName}
                onTouchTap={e => router.push(`/claims/${claim.id}`)}
                onAuthorTouchTap={e => router.push(`/users/${claim.authorId}`)}
              />
            )
          }
        </List>
      </CardText>
    </Card>
  )
}

export default GetterHOC(
  Argument,
  (props) => ({
    argument: {
      path: props.location.pathname + '/page',
      mapResponseToProps: (resp) => ({page: resp})
    }
  })
)
