pub trait Parse
where
    Self: Sized,
{
    type Error;
    type Parser;

    fn parse(parser: Self::Parser) -> Result<(Self::Parser, Self), Self::Error>;
}
