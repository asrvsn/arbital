import React, { Component, PropTypes } from 'react'

import {Card, CardActions, CardHeader, CardMedia, CardTitle, CardText} from 'material-ui/Card';
import {List, ListItem} from 'material-ui/List';
import FlatButton from 'material-ui/FlatButton';
import {Tabs, Tab} from 'material-ui/Tabs'

import AuthoredListItem from '../items/AuthoredListItem';
import GetterHOC from '../hoc/GetterHOC';

const styles = {
}

class Claim extends Component {

  constructor(props) {
    super(props)
    this.state = {
    }
  }

  render() {
    const { page, router } = this.props
    const { claim, argsFor, argsAgainst } = page

    const goToArg = (id) => router.push(`/arguments/${id}`)
    const goToUser = (id) => router.push(`/users/${id}`)

    const getArgsList = (args) => (
      <List>
        { (args.length > 0) ?
            args.map(arg =>
              <AuthoredListItem
                key={arg.id}
                text={arg.text}
                authorId={arg.authorId}
                authorName={arg.authorName}
                onTouchTap={e => goToArg(arg.id)}
                onAuthorTouchTap={e => goToUser(arg.authorId)}
              />
            )
          :
            <ListItem primaryText="(nothing)" />
        }
      </List>
    )

    return (
      <Card>
        <CardTitle
          title={claim.text}
          subtitle={`${claim.authorName} created on ${claim.creationDate}`}
        />
        <CardActions>
          <FlatButton label="Add argument" />
          <FlatButton label="Update" />
          <FlatButton label="Delete" />
        </CardActions>
        <CardText>

          <Tabs>
            <Tab label="For" >
              { getArgsList(argsFor) }
            </Tab>
            <Tab label="Against" >
              { getArgsList(argsAgainst )}
            </Tab>
          </Tabs>

        </CardText>
      </Card>
    )
  }
}

export default GetterHOC(
  Claim,
  (props) => ({
    claim: {
      path: props.location.pathname + '/page',
      mapResponseToProps: (resp) => ({page: resp})
    }
  })
)
